---
title: Bridge Lifecycle
summary: The GodotEguiBridge node that hosts an egui runtime inside a Godot scene tree, and the frame lifecycle callers drive through it.
---

# Bridge Lifecycle

`gdext-egui` exposes a single Godot node that owns one egui runtime. Adding that node to a
scene tree gives the surrounding Godot code an `egui::Context` it can draw into every frame.
This document covers the node itself, its tree lifecycle, and the frame entry points. Viewport
management, widget callbacks, input translation, and rendering are covered in their own specs.

## Bridge Node {#260730-godot-egui-bridge-node}

The crate exports one Godot class, registered under the name `GodotEguiBridge`. It derives from
`CanvasLayer`, so egui output is composited above the scene's normal 2D content. The class is a
`tool` class: it runs in the Godot editor as well as at runtime, which is what makes editor
plugin usage possible.

From Rust the class is the type `EguiBridge`, re-exported at the crate root. Callers obtain an
instance the usual gdext way — instantiating it (`EguiBridge::new_alloc()`) and adding it to the
tree, or placing a `GodotEguiBridge` node in a scene from the editor. Nothing else in the crate
is publicly instantiable; the per-viewport surface node is registered as an engine-internal class
and never appears in the editor's class list.

One bridge owns exactly one egui context. Multiple bridges in the same scene are independent
runtimes and do not share state.

## Tree Lifecycle {#260730-bridge-tree-lifecycle}

The bridge initializes when it enters the tree and tears down when it leaves.

- **Entering the tree** installs the egui repaint hook so that repaint requests raised from
  anywhere in egui reach this bridge, and puts the context into non-embedded viewport mode so
  viewports may become real OS windows.
- **Leaving the tree** releases the bridge's deferred-work channel. Rendering resources owned by
  each viewport surface are freed by that surface when it leaves the tree.

Initialization is also lazy: if the first frame is requested before the enter-tree callback has
run, the bridge initializes on the spot. A caller therefore never has to sequence its own setup
against the node's tree callbacks.

Repaint requests that arrive after the bridge has been disposed are dropped with a printed
notice rather than crashing.

## Starting a Frame {#260730-current-frame-entry}

`current_frame()` is the primary entry point. It returns the `&egui::Context` for the frame,
starting a new frame first if one is not already open. Everything drawn on the returned context
appears on the bridge's root canvas.

The call is idempotent within a frame: calling it repeatedly during the same Godot process tick
returns the same open frame rather than restarting one. Typical use is to call it once per
`process()` and draw immediately.

Cloning the returned context and using it outside the frame it came from is not supported. The
context must only be touched on the main thread, immediately after the call that produced it.

## Frame Advance {#260730-frame-advance-on-process}

The bridge drives egui from its own `process()` callback. Each tick, in order:

1. If a repaint was requested since the last tick, a frame is started automatically — even when
   no caller invoked `current_frame()` this tick. This is what keeps animations, tooltips, and
   hover effects alive on frames where game code draws nothing new.
2. If a frame is open — whether opened by step 1 or by a caller's `current_frame()` — it is
   finished: widget callbacks for frame end run, viewports are reconciled, texture updates are
   applied, and every viewport that produced output this frame is painted.

Because the frame closes inside the bridge's own `process()`, callers must draw during their own
`process()` and must not hold the context past it. A caller whose node processes after the
bridge will have its drawing applied to the following frame.

egui's frame clock is fed from the engine's tick counter, and keyboard modifier state is sampled
from the engine at frame start, so modifiers are correct even for frames with no key event.

## Context Configuration {#260730-setup-context-deferred}

`setup_context(f)` runs `f` against the `egui::Context` at a point where mutating context-wide
state is safe. If no frame is currently open the closure runs immediately; if a frame is open it
is deferred and runs at a later frame boundary.

This is the supported way to apply settings that must not change mid-frame — style, fonts, zoom
factor, and similar. Callers do not need to know whether a frame happens to be open at the call
site.

## Main Thread Contract {#260730-main-thread-contract}

The bridge is main-thread-only. Starting a frame from any thread other than the one the bridge
was created on aborts the process with an assertion failure; this is a programming-error check,
not a recoverable condition.

Internally, work that originates off the main thread is routed back to the main thread through
Godot's deferred-call mechanism instead of touching the context directly, so an off-thread
repaint request is safe even though an off-thread draw is not.

## Texture Size Limit {#260730-max-texture-bits-export}

`max_texture_bits` is an exported integer property, editable from the Godot inspector and
readable/writable from GDScript. It sets the largest texture edge egui may allocate, expressed as
a power of two: the reported maximum texture side is `2^max_texture_bits`.

The value is clamped to the range 8–16 when applied, giving an effective texture-side range of
256–65536. The default is 13 (8192).

> [!note] Implementation Gap · 2026-07-30
> Unexposed capability. The bridge registers one `#[func]` method
> (`__internal_try_start_frame_inner`) that is reachable from GDScript because Godot's deferred
> call mechanism requires a registered method name. It is not part of the supported API and
> carries no stability guarantee; there is no decision yet on hiding it from the scripting
> surface.
