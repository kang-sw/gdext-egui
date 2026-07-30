---
domain: viewport-lifecycle
description: "Realizing an egui viewport as Godot nodes - the validate/start/end triple, the split viewport/surface tables, the close handshake, and the lock that must not be held across Godot calls."
sources:
  - src/context/
related:
  frame-lifecycle: "frame.rs is the only caller of every function here and fixes the order they run in."
  surface-rendering: "Surfaces are created and freed here; painting consumes what viewport_end_frame stores."
  input-routing: "Root and spawned viewports are wired to opposite Godot input paths during validation."
---

# Viewport Lifecycle

## Entry Points

- `src/context/viewport.rs` - `viewport_validate`, `viewport_start_frame`, `viewport_end_frame`
  are the whole domain; `apply_viewport_commands` and `sync_viewport_info` hang off validation.
- `src/context/mod.rs` - the close-state constants and the doc comment above them are the only
  written description of the close handshake.
- `src/context/frame.rs` - the sole caller. This domain cannot be modified safely without it.

## Module Contracts

- Per viewport id, the mandatory triple is **validate -> start -> end**, once each per frame,
  nested LIFO with any inner viewport's triple. Start unwraps the entry validation inserts;
  end unwraps both the viewport entry and its surface. `begin_pass`/`end_pass` live at the two
  ends of the triple, so an early `return` inserted between them desynchronizes egui's pass
  stack.
- **Two tables, one concept.** The viewport record lives in a mutex-guarded map and its Godot
  nodes in a separate `RefCell` map. Keys of the surface map must stay a subset of the viewport
  map: the paint loop unwraps the viewport entry for every surface, and disposal asserts the
  viewport entry existed. Create and remove them in lockstep.
- `viewport_validate` **checks the surface out of the map for the duration of the call** and
  checks it back in at the end. Anything that runs in between - a Godot signal fired by
  `grab_focus`, `set_visible`, `add_child` - sees a hole where the surface should be.
- The ROOT viewport is validated **twice per frame**: once at frame start and again in the
  `finish_frame` drain loop, because ROOT also appears in egui's viewport output. Validation
  must stay idempotent.
- Freeing a `SurfaceContext` by dropping it leaks the Godot nodes. `free_surface` is the only
  correct destructor; adding a node-holding field to that struct means extending it.

## The Lock That Must Not Escape

`viewport_validate` holds the viewport-map mutex across both `apply_viewport_commands` and
`sync_viewport_info`, which together make dozens of synchronous Godot window calls. The mutex
is egui's (`parking_lot`, non-reentrant). Any Godot call that synchronously emits a signal
reaching the painter's event path re-enters the repaint callback, which takes the same lock -
a hard self-deadlock (10 s debug panic, permanent hang in release).

This is the documented reason the `"resized"` signal routes through a deferred command instead
of calling the egui context directly; the code comment says it "actually deadlocks on widget
initialization". **When extending `apply_viewport_commands`, any new Godot call that can fire a
signal must be deferred, not made inline.**

The field comment stating the lock order (viewport map before surface map) is still accurate in
intent, but the paint loop in `frame.rs` already violates it - see that domain's debt list.

## Close Negotiation

A four-state atomic, with its transitions spread across three functions and one Godot signal
callback {#260730-close-negotiation}. What makes the handshake resolvable inside one frame is
the `finish_frame` drain loop: it re-drains egui's viewport output at the top of every
iteration, so a viewport's own `CancelClose` output re-enters the queue and earns a second
validation in the same frame. The close check in that loop runs *before* the validation of the
same iteration, so a command emitted during an iteration is only honored on the next one.

The load-bearing ordering is that validation runs **before** the frame start that injects
egui's close event. Reverse them and the promotion to the terminal state happens in the same
iteration that injected the event, so the drain loop disposes the viewport before a queued
`CancelClose` ever reaches command application - cancellation silently stops working.

Disposal itself is **by omission**: nothing calls "destroy viewport". A viewport that is not
re-shown this frame simply is not removed from the surviving set, and `finish_frame` frees it.

## Extension Points & Change Recipes

- **Support a new `ViewportCommand`**: the match in `apply_viewport_commands`. Many arms are
  empty or `TODO` today {#260730-viewport-commands}; an unhandled arm is a silent no-op, not a
  warning.
- **Report a new `ViewportInfo` field**: `sync_viewport_info` assigns each field individually
  onto a viewport info record that is **never cleared between frames**
  {#260730-viewport-info-sync}. A field nobody assigns is simply never set, and a field that
  stops being assigned keeps its stale value rather than reverting to unset.
- **Honor a new `ViewportBuilder` field**: either rely on the builder patch emitting a command,
  or add explicit creation-time handling next to the existing flag handling. The
  recreation-only flag list in the comment there is aspirational - see debt below.
- **(planned)** The `TODO: Merge IGNORE behavior between non-root and root` in the root wiring
  branch is the prerequisite for `viewport_spawn_as_child` and `attach_node_to_viewport`, both
  of which are stubs.

## Common Mistakes

- Assuming `ViewportBuilder` changes take effect after creation. The recreate flag from the
  builder patch is explicitly discarded, and only `active` and `titlebar_shown` are read at
  creation. Everything else in the "recreation-only" comment is a silent no-op
  {#260730-native-window-defaults}.
- Assuming windows can leave a mode they entered. `Minimized(false)`, `Maximized(false)` and
  `Fullscreen(false)` are all empty arms - there is no path back to windowed.
- Expecting a repaint request for a viewport that has not been validated yet to survive. It is
  warned about and dropped.
- Adding a "run this every frame" feature without setting the repaint flag. Frame start parks
  each viewport's repaint deadline an hour into the future, and deferred viewports only re-run
  their UI callback once that deadline is in the past.

## Technical Debt

- **The rebuild path is dead, and would leak if revived.** The only trigger for rebuilding an
  existing viewport is a parent change, but the parent field of the viewport info is never
  written anywhere, so it never fires. If it did, the rebuild branch drops the old surface
  plainly instead of calling `free_surface` - leaking a painter and a window - and swaps the
  event channel, discarding any queued input (a viewport rebuilt mid-drag would never see the
  button release). Any future "recreate viewport" feature walks into all three.
- **Check-in uses insert-if-absent.** If the surface slot is occupied on check-in (only
  reachable through re-entrancy), the freshly built surface is dropped without being freed.
- **The root close warning is unreachable.** Command application skips every viewport without a
  window, which is exactly the root case, so the "Root viewport received close request!" branch
  can never execute.
- **IME is written twice per frame with different intent.** Frame end sets the IME active flag
  from whether egui produced an IME request, on the same window `IMEAllowed` targets, and it
  runs last in the triple - so an `IMEAllowed(true)` with no active egui IME request is
  reverted to `false` later in the same frame. For ROOT, `IMEAllowed` is never applied at all,
  since command application requires a window.
- **`InnerSize` resizes the wrong thing** (marked `FIXME` in place): it sets the containing
  window's size while info reporting measures the painter, so request and report disagree by
  the decoration thickness.
- **Cursor arbitration is order-dependent.** Each viewport's frame end writes into one shared
  cursor slot; with several viewports requesting different cursors, the winner depends on the
  drain order of egui's viewport output {#260730-cursor-shape-sync}.
