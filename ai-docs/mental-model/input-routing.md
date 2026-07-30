---
domain: input-routing
description: "Getting Godot input into egui - the two routing modes and how one of them silently collapses, the one-frame-stale consumption decision, and which egui viewport the consume predicates actually read."
sources:
  - src/surface/
related:
  viewport-lifecycle: "Root and spawned viewports are wired to opposite Godot input paths during validation, and the event channel is created there."
  surface-rendering: "Input coordinates are divided by the UI scale that draw() cached, so input trails rendering by a frame."
---

# Input Routing

## Entry Points

- `src/surface/input.rs` - `try_consume_input` is the entire Godot-to-egui translation; the
  key and modifier tables sit beside it.
- `src/surface/mod.rs` - the Godot virtual hooks and `on_event`, which is the only egress.
- `src/context/viewport.rs` - `viewport_validate` decides which routing mode a surface gets.

## Module Contracts

- **Two routing modes are intended** {#260730-input-routing}: the root surface takes the global
  input path with an ignore mouse filter, and spawned surfaces take the GUI input path with a
  pass filter. For the root, the ignore filter and the GUI-hook early-return are belt-and-braces
  - an ignored control is never handed GUI input in the first place. The spawned mode, however,
  does not survive contact with the engine; see debt.
- **Consumption decisions read *last* frame's egui state.** The translated event is only pushed
  onto a channel; egui does not see it until the next frame start drains that channel
  {#260730-input-consumption}. Anything expecting same-frame feedback - "consume this click
  because it just landed on a window that appeared this frame" - is wrong by one frame.
- **The consume predicates do not resolve to the viewport that received the event.** They are
  queried on the shared context outside any pass, and egui resolves those to ROOT when the pass
  stack is empty, while its memory-backed predicates resolve to whichever viewport began a pass
  last. So a spawned surface's motion-consume decision is driven by root state, and the two
  halves of the want-pointer-input predicate can read different viewports.
- **`on_event` also requests a repaint** {#260730-focus-pointer-notifications}. Any new path
  that injects events without going through it leaves the UI frozen until some other repaint
  source fires. Note the deliberate contrast with the resize signal, which must *not* touch the
  context directly - see `viewport-lifecycle`.
- The painter's initialization runs **before** the node is added to the tree, so its context and
  event sink exist before `ready` and the first input tick. That same pre-insertion window is
  what silently discards the spawned-mode input-processing guard (below).

## Extension Points & Change Recipes

- **Map a new key**: the key table, whose ~100 commented-out arms are a coverage backlog, not
  dead code to delete {#260730-key-mapping-coverage}.
- **Handle a new event type**: `try_consume_input` is a hand-rolled downcast cascade whose every
  arm returns. Preserve that shape - falling through re-tests the event against later casts.
  Order is load-bearing: motion and button events are both subclasses of the mouse event base,
  so a branch on the base class must never be inserted above them.

## Common Mistakes

- Assuming other global handlers still see a consumed event. Marking input handled aborts the
  remaining global-input dispatch for that pass, and the engine walks that group in reverse
  order - so only handlers already visited saw it. The bridge's position in the scene tree
  decides who those are.
- Pushing events into the shared raw-input template. It is cloned per viewport at frame start,
  so anything placed there is replayed into *every* viewport.
- Assuming a canceled mouse button releases anything. A canceled press returns without emitting,
  leaving egui believing the button is still down.
- Overlooking that consuming *motion* also marks it handled engine-wide, which stops other Godot
  controls from updating hover state while the cursor is over egui.

## Technical Debt

- **The spawned-viewport routing mode collapses into the root mode.** Validation calls
  `set_process_input(false)` on the painter *before* inserting it into the tree, and Godot
  re-enables global input processing at `NOTIFICATION_READY` for any node that overrides the
  input virtual - which gdext always registers. So spawned painters run the global path too,
  and its unconditional else-branch pins their mouse filter to ignore on the first event they
  see. Consequences: the GUI-input path, the drop hooks, and mouse enter/exit notifications are
  dead for **all** viewports, not just the root. The spec describes root and spawned viewports
  as taking different paths, and describes Godot-to-egui drops as working
  {#260730-godot-drop-into-egui}. *Confirming this requires running a Godot instance; it is
  derived from reading engine and binding source.*
- **Root hover state sticks.** Following from the above, the pointer-gone event is never
  delivered, so hover highlights stay lit when the pointer leaves. Focus notifications still
  work, because focus is grabbed programmatically on click.
- **Ctrl+wheel zoom is scaled by the wheel factor rather than by notches.** The zoom exponent is
  the raw wheel delta, which defaults to 4 when the platform reports none, so one notch is a
  16x zoom step - the adjacent comment says the value was meant to be in the range -1..1
  {#260730-wheel-and-zoom}.
- **Text input can panic.** The character is converted from the raw Unicode value with only a
  lower bound checked, so a surrogate or out-of-range value unwraps on `None`
  {#260730-keyboard-text-input}.
- **Pointer coordinates ignore canvas transforms, and double-subtract on the GUI path.** The
  offset comes from the control's global position, but the bridge is a `CanvasLayer` - any
  offset, scale, or rotation on that layer puts the event position and the offset in different
  spaces; the transform-with-canvas variant is the correct source. The same subtraction is
  applied on the GUI path, where Godot has already made the position control-local, which is
  harmless only while spawned painters sit at the origin of their own window
  {#260730-pointer-input}.
- **Modifiers are computed twice, differently** {#260730-modifier-mapping}: per event from the
  event's own mask, and per frame by polling the engine. Fixing one leaves the other
  inconsistent, and the per-frame one is what egui code reading the context observes.
- **(planned)** Dragging *out of* egui does not exist - the drag-data hook returns nil with
  TODOs for both directions. The physical-key field is filled from the logical keycode as a
  placeholder. IME composition text is never read on the input side.
