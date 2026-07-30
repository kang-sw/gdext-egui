---
domain: frame-lifecycle
description: "The one-frame-at-a-time state machine on EguiBridge - who opens a frame, what must happen inside it, and in what order finish_frame tears it down."
sources:
  - src/context/
  - src/surface/
related:
  viewport-lifecycle: "Every frame phase delegates the per-viewport validate/start/end triple."
  widget-callbacks: "Both callback groups run inside the ROOT pass; moving either across a pass boundary silently drops their output."
  surface-rendering: "finish_frame owns the texture-create -> paint -> texture-free ordering that draw() depends on."
---

# Frame Lifecycle

## Entry Points

- `src/context/frame.rs` - `try_start_frame` and `finish_frame` are the whole state machine.
- `src/context/mod.rs` - `ICanvasLayer::process` is the only automatic driver; `SharedContext`
  holds the frame-claim flag and the repaint wake flag.

## Module Contracts

- `SharedContext::try_advance_frame` is a *consuming* claim (`swap(true)`), not a query.
  Whoever it returns `true` to owes the rest of the start sequence, because `process` runs
  `finish_frame` on any tick where `is_in_frame()` holds, and `finish_frame` unconditionally
  issues egui's `end_pass` for ROOT. A claim taken but not redeemed becomes an unbalanced
  `end_pass`.
- The bridge opens a frame **on its own initiative** only when the repaint flag is set; frames
  opened by user code are finished and painted on the next tick regardless of that flag
  {#260730-frame-advance-on-process}. So any new "state changed, redraw" path that does not
  itself call into the context must set the flag, or the change stays invisible until
  something else wakes the bridge {#260730-registration-wakes-repaint}.
- egui's repaint callback sets the flag **only for viewports already present in the viewport
  map**; for an id whose first validation has not run yet it warns and drops the wake.
- A frame opened by user code in `process()` is closed on the *next* tick when that node is
  processed after the bridge. "One `current_frame()` call equals one finished frame this
  tick" is not guaranteed {#260730-current-frame-entry}.
- Everything between `viewport_start_frame(ROOT)` and `viewport_end_frame(ROOT)` draws into
  the ROOT pass. Immediate viewports nest inside it; deferred viewports are rendered after
  ROOT's `end_pass` and are therefore top-level passes.

## Ordering Rules

Inside `try_start_frame`:

1. `try_initiate` must precede any `viewport_validate` - validation unwraps the
   deferred-command sender that `try_initiate` creates.
2. The raw-input template's viewport map is *overwritten* wholesale here, while
   `viewport_validate` *inserts* into it. Filling the template after validation clobbers
   fresh per-viewport info {#260730-viewport-info-sync}.
3. `viewport_validate(ROOT)` must precede the root-region sync block and
   `reset_root_region_sync`; both unwrap the ROOT surface {#260730-root-region-sync}.

Inside `finish_frame`:

1. Spawned viewports must be re-registered via `show_viewport_deferred` **before** ROOT's
   `end_pass`. After it, egui emits no output for them and step 2 destroys them.
2. Viewport disposal is *by omission* - a viewport that produced no output this frame falls
   out of the surviving set and is freed. Surface first, then viewport entry.
3. Textures: create, then paint, then free. Freeing before painting sends `draw` down its
   missing-texture path, which abandons every remaining primitive of that surface rather than
   skipping one - the viewport goes blank or partial {#260730-texture-management}.
4. `share.finish_frame()` is last. An early `return` added above it latches the frame claim
   forever and the bridge silently stops rendering.

## Coupling

- `frame.rs` <-> `viewport.rs`: close negotiation is one state machine split across four
  functions {#260730-close-negotiation}.
- `frame.rs` <-> `surface/mod.rs`: the UI scale (egui's `pixels_per_point`, despite the field
  doc calling it a user zoom) is written by `viewport_end_frame`, divided into the next
  frame's screen rect by `viewport_start_frame`, passed to `draw`, applied to vertices and
  clip rects there, and cached for the input path {#260730-ui-scale-application}. Five sites,
  one convention - change one and hit-testing silently diverges from what is drawn.
- `process` calls `handle_bg_message` both before and after the frame on purpose. Draining a
  deferred repaint synchronously re-enters egui's repaint callback, which sets the wake flag;
  the leading call runs **before** the flag is consumed, so it produces a frame in the same
  tick, while anything the trailing call drains can only be seen on the next tick. Deleting
  the leading call costs a frame per resize signal that arrives between ticks; deleting both
  disables resize-driven repaint entirely.
- `try_dispose`/`try_initiate` <-> surfaces built in an earlier tree session: those surfaces
  captured a clone of the *old* deferred-command sender. After `exit_tree` + `enter_tree` the
  senders are dead and resize repaints vanish with no error {#260730-bridge-tree-lifecycle}.

## Extension Points & Change Recipes

- **Defer work off a lock or onto the main thread**: add a `DeferredCommand` variant plus an
  arm in `handle_bg_message`. This is the sanctioned escape hatch - see `viewport-lifecycle`
  for why calling into the egui context from a Godot signal handler deadlocks.
- **Map a new egui cursor**: the icon match in `finish_frame`. Unmapped icons degrade to the
  arrow silently {#260730-cursor-shape-sync}.
- **(planned)** `viewport_spawn_as_child` and `attach_node_to_viewport` are stubs.

## Common Mistakes

- Treating the deferred-command channel as cross-thread machinery. The comments say "spawn"
  and "join background worker thread", but nothing is spawned - it is a same-thread deferral
  queue. Threading was removed in `5b52d98`.
- Capturing anything richer than a plain `InstanceId` in the immediate-viewport renderer
  closure. It lives in egui's thread-local storage and outlives the Godot binding; dropping a
  `Variant` after engine shutdown panics. The scar is commit `0556b23`.
- Assuming one bridge per process is enforced. `set_immediate_viewport_renderer` is a global
  slot won by whichever bridge started a frame last; the loser's immediate viewports render
  nothing and log nothing.
- Calling `sync_root_region(None)` before the first frame. It unwraps the ROOT surface, which
  only exists once a frame has started. The `Some` arm has no such precondition, so the two
  arms of one public function differ in what they require.
- Reaching an **immediate viewport** from a frame the bridge opened by itself. The immediate
  renderer takes a shared bind on the bridge, while `process` already holds an exclusive one -
  so `show_viewport_immediate` from a widget callback panics inside library code, not user
  code, and only on the idle frames the bridge drives. The same asymmetry applies to any user
  callback that binds the bridge {#260730-main-thread-contract}.

## Technical Debt

- **Off-main-thread frame start is broken.** `queue_try_start_frame` consumes the frame claim
  itself and then defers the real start, which finds the claim already taken and returns
  without beginning a pass. The next `process` then ends a pass that was never begun.
  `viewport_spawn` is documented as thread-safe and is the public door into this path
  {#260730-viewport-spawn-deferred}.
- **`try_dispose` does not clear the frame claim.** Leaving the tree mid-frame latches it;
  re-entering goes straight to an unbalanced `end_pass`.
- **The paint loop inverts the documented lock order.** The surface map's borrow guard lives
  for the whole `for` loop (temporary lifetime extension), and the body then locks the
  viewport map - the reverse of the order stated on the field. Since the surface map is a
  `RefCell` and the viewport map a mutex, the failure is a `BorrowMutError` panic on any
  re-entrant surface access reached from `draw`, not a hang.
- **`setup_context` does not defer the way its doc claims.** Scripts held back because a frame
  was open are drained at the end of *every* viewport pass, including nested immediate ones,
  so they can run while the ROOT pass is still open {#260730-setup-context-deferred}.
