---
title: Rendering
summary: How egui's tessellated output becomes visible Godot content — canvas items, clip-rect scissoring, texture upload, UI scaling, and cursor shape.
---

# Rendering

At the end of each frame egui produces tessellated meshes with clip rectangles. `gdext-egui`
turns that output into Godot canvas content and applies the accompanying platform output such as
the cursor shape. Everything here is observable as what appears on screen; none of it requires
caller action.

## Canvas Item Rendering {#260730-canvas-item-rendering}

Drawing goes straight to Godot's rendering server rather than through scene-tree draw calls. Each
clipped primitive egui emits becomes one canvas item parented to the viewport surface's own
canvas item, with its draw index set to the primitive's position in egui's output so painter
order is preserved exactly.

Because the content lives in canvas items rather than nodes, egui UI does not appear in the scene
tree, cannot be inspected as nodes in the remote debugger, and is unaffected by node-level
operations. Its position in the overall draw order is determined by the bridge's `CanvasLayer`.

Vertex colors and UVs are passed through to Godot as-is, so egui's own styling — including
transparency and texture atlases — renders unmodified.

> [!note] Implementation Gap · 2026-07-30
> Missing behavior. Only mesh primitives are drawn. egui's custom paint callbacks are not
> supported: encountering one logs an error and skips that primitive, so widgets built on custom
> painting render as nothing.

## Canvas Item Reuse {#260730-canvas-item-pooling}

Canvas items and their materials are pooled across frames. A frame that needs more primitives than
the last one allocates the extra items; a frame that needs fewer frees the surplus. Reused items
are cleared before redrawing.

All items, materials, and the shared shader are released when the surface leaves the tree, so
closing a viewport or removing the bridge does not leak rendering-server resources.

## Clip Rect Scissoring {#260730-clip-rect-shader}

egui clips widget content to rectangles — scroll areas, windows, and panels all rely on it.
Rather than Godot's built-in clipping, each primitive is drawn with a small canvas-item shader
that receives the primitive's clip rectangle as a uniform and discards fragments outside it.

This is per-fragment clipping, so it is exact at rectangle edges and works for overlapping and
nested clip regions without depending on node hierarchy. The shader is created once per surface
on first draw and shared by every primitive of that surface, with only the uniform differing.

## UI Scale {#260730-ui-scale-application}

egui lays out in logical points; the surface converts to physical pixels by multiplying vertex
positions and clip rectangles by the frame's pixels-per-point. Zoom changes and high-DPI displays
therefore affect the rendered size without changing egui-side layout coordinates.

The same scale is retained for the next input frame, which is what keeps pointer positions and
rendered geometry in agreement after a zoom change.

## Texture Management {#260730-texture-management}

egui's texture deltas are applied before painting each frame.

A full texture upload converts egui's color image to unmultiplied sRGBA and creates a Godot image
texture from it. A partial update blits the changed region into the CPU-side copy of the existing
texture and then pushes that copy to the GPU, so incrementally growing atlases — the font atlas in
particular — do not force a full re-upload each time a new glyph is rasterized.

Textures egui frees are dropped from the library; they are reference-counted, so they disappear
once nothing else holds them.

Failures are reported rather than silent: an image that cannot be created, or a mesh referencing a
texture that is not in the library, produces a message in the Godot output.

> [!note] Implementation Gap · 2026-07-30
> Missing behavior. When a mesh references a texture that is not in the library, the surface
> abandons the entire remaining draw for that viewport rather than skipping the one primitive. A
> single missing texture therefore blanks every primitive after it for that frame.

## Per-Frame Painting {#260730-per-frame-painting}

Only viewports that actually produced output during the frame are repainted. A viewport whose
draw code did not run keeps its previous canvas content on screen unchanged rather than being
cleared.

This is what makes idle UI cheap: with nothing requesting a repaint, no meshes are re-uploaded and
no canvas items are rebuilt, while the UI stays visible.

## Cursor Shape {#260730-cursor-shape-sync}

The cursor icon egui requests is applied to the platform cursor once per frame, after all
viewports have drawn. egui's icons map onto Godot's cursor shapes: text and vertical-text both
become the I-beam, the various resize icons become the matching directional resize cursors, and
icons with no Godot counterpart fall back to the arrow.

Two rules keep egui from fighting the rest of the game over the cursor:

- A hidden or default cursor request never overwrites a meaningful cursor another viewport already
  requested this frame.
- A request to hide the cursor entirely is ignored, so egui cannot leave the game with an
  invisible cursor after the UI goes away.

Frames where egui requests nothing leave the cursor alone, so non-egui code retains control of the
cursor whenever the pointer is not over egui UI.
