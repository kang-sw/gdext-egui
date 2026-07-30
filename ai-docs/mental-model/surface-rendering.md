---
domain: surface-rendering
description: "Turning egui's tessellated output into RenderingServer canvas items - the texture library, the per-primitive clip shader, RID pooling, and the scale that ties painting to hit-testing."
sources:
  - src/surface/
related:
  frame-lifecycle: "finish_frame owns the texture-create -> paint -> texture-free ordering and decides which viewports repaint."
  viewport-lifecycle: "Surfaces are created and destroyed there; primitives and UI scale are stored by viewport_end_frame."
  input-routing: "draw() is the only writer of the cached UI scale that input coordinates are divided by."
---

# Surface Rendering

## Entry Points

- `src/surface/mod.rs` - `TextureLibrary`, `EguiViewportBridge::draw`, and the clip shader
  source are the whole domain.
- `src/context/frame.rs` - `finish_frame` is the only caller of `draw`, `update_texture`, and
  `free_texture`.

## Module Contracts

- **Drawing is not "clear and redraw" every frame.** Only viewports that produced output are
  painted; a viewport whose UI callback did not run keeps its existing canvas items untouched
  {#260730-per-frame-painting}. Skipping `draw` is the normal idle path, not an error path.
- **`draw` must receive the same scale used to tessellate.** It also caches that scale for the
  input path {#260730-ui-scale-application}. The scale appears at five sites - screen rect
  computation, vertex positions, clip rects, and the input cache - and they must agree or
  hit-testing silently diverges from what is on screen. In particular, an early `return` added
  before the cache assignment desyncs input after any zoom change.
- **The canvas-item and material vectors are positionally parallel**, and each item's draw
  index is assigned **only at creation**, never on reuse {#260730-canvas-item-pooling}. The
  pool is therefore append-and-truncate-tail only. Any change that inserts, removes, or
  reorders mid-vector breaks painter order with no error at all.
- **RIDs are not refcounted.** `exit_tree` is the only full teardown and `draw`'s shrink loop
  the only incremental free, so a new pooled RID field needs an arm in *both* or it leaks. The
  Godot image and texture handles, by contrast, are refcounted and need no explicit free.
- **The paint loop constrains this domain from the outside.** It holds the surface map's borrow
  guard for the whole loop, so any re-entrant surface access reached from `draw` panics; and it
  unwraps the viewport entry for each surface, so a surface outliving its viewport record
  panics rather than skipping. Cloning a surface record shares the Godot handles rather than
  duplicating them - the clone the loop iterates paints the same node.

## Extension Points & Change Recipes

- **Support egui paint callbacks**: the primitive match in `draw` handles meshes only and logs
  an error for callbacks {#260730-canvas-item-rendering}. This is the seam.
- **Support another image format**: the format match in `update_texture` has exactly one live
  arm; the `#[cfg(any())]` arm is dead, not a template.
- **Change the clip shader**: the uniform name is duplicated as a string literal at the
  parameter-set call site. A mismatch fails **silently and catastrophically** - the uniform
  keeps its zero default, which makes the clip rectangle empty, so every fragment is discarded
  and the viewport renders blank {#260730-clip-rect-shader}.

## Common Mistakes

- **Dropping `COLOR *= texture(TEXTURE, UV)` from the fragment shader.** The custom shader
  replaces Godot's default, so that line is what textures the UI at all; without it every
  widget renders as flat untextured color.
- **Adding a transform to a pooled canvas item.** The shader compares the raw pre-transform
  vertex against a clip rect computed in the same space. Any per-item transform, or reparenting
  under a non-identity transform, misaligns clipping without an error.
- **Omitting the clear on the reuse branch.** Geometry then accumulates across frames, which
  reads as ghosting rather than as a failure.
- **Creating RIDs eagerly in `ready()`.** Leaving the tree drains the pools and drops the
  shader; re-entering works only because `draw` recreates them lazily.
- **Treating the underscore-prefixed clear method as live teardown.** Nothing calls it;
  texture teardown relies entirely on handle refcounting when the bridge drops.
- Assuming the texture library is per viewport. It is one map on the bridge shared by all
  viewports; canvas items are per surface, textures are global.

## Technical Debt

- **One missing texture blanks the rest of the viewport.** The missing-texture path `return`s
  out of the primitive loop instead of continuing, and by then the canvas items have already
  been cleared, so every primitive after the first miss disappears for that frame
  {#260730-texture-management}.
- **Partial texture updates unwrap.** An incremental delta for an id that is not in the map
  panics. This is reachable: image creation failure aborts both branches and texture creation
  failure aborts the full-upload branch, either returning before the map is populated, and the
  next incremental font-atlas delta then hits the unwrap. The symmetric free path only warns,
  so the two paths differ in robustness for no stated reason.
- **The payload buffer is sized from the image's reported bytes-per-pixel but written with a
  hardcoded four-byte stride.** These agree only while a single 8-bit RGBA format exists.
- **Idle viewports can reference freed textures.** Canvas items retain the texture RID from a
  previous frame, and texture frees run after painting - a texture freed while a
  non-repainting viewport still displays it leaves that item pointing at a released resource.
- Clipping is `discard`-based and boundary-inclusive (the discard test is strict, so fragments
  exactly on the edge survive), which differs at half-pixel edges from the built-in scissor it
  replaced in `8298b55` ("fix clipping glitch"). Compare against that commit before reworking
  edge behavior.
