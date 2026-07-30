---
title: "Off-main-thread frame start consumes the frame claim without beginning a pass"
sage-review-design: recommended
spec:
  - 260730-viewport-spawn-deferred
  - 260730-main-thread-contract
related-mental-model:
  - frame-lifecycle
---

# Off-main-thread frame start consumes the frame claim without beginning a pass

## Background

`try_advance_frame` is a compare-and-set claim, not a query — whoever it returns true to
owes the rest of the frame-start sequence. Grep `queue_try_start_frame` in
`src/context/frame.rs`: on the non-main-thread branch it takes the claim itself and then
defers the real start through the bridge's single `#[func]`. When that deferred call
lands, `try_start_frame` calls `try_advance_frame` again, finds the claim already taken,
and returns without ever issuing `begin_pass`.

The next `process` tick then sees an open frame and runs `finish_frame`, which ends the
ROOT pass unconditionally. egui receives an `end_pass` with no matching `begin_pass`.

`viewport_spawn` is documented as callable from any thread and is the public door into
this path, so the failure is reachable from the API as specified rather than only from
crate-internal misuse.

Two adjacent facts belong in the same investigation:

- `try_dispose` does not clear the claim. Leaving the tree mid-frame latches it, and
  re-entering goes straight to the same unbalanced `end_pass`. Different trigger, same
  broken invariant, and any fix that makes the claim self-describing should cover both.
- Taking `self.to_gd()` from a non-main thread is separately questionable under gdext.
  Whether the deferred-call hop is sound at all is part of what this ticket has to
  settle — not an aside.

## Decisions

Not yet made. This is the one item in the current defect set where the fix is a design
choice rather than a correction, which is why it carries a design-review posture.

Candidate directions, none selected:

- Do not claim in `queue_try_start_frame`; let the deferred call make the claim. Smallest
  change, but leaves a window where two off-thread callers both defer.
- Make the claim carry who holds it, so a redemption by a different path is detectable
  instead of silently skipped.
- Reject off-thread frame start outright and narrow the documented thread-safety of
  `viewport_spawn` to match. This is a spec change, not just a fix, and needs owner
  intent — the current wording is a promise the crate makes to downstream users.

The third option is genuinely on the table: the crate is single-threaded by construction
(`_non_send_sync`, two `unsafe impl Send` blocks justified only by "everything stays on
the main thread"), so a thread-safe entry point may be a promise that should never have
been made.

## Phases

### Phase 1: Establish a reproduction

Drive `viewport_spawn` from a non-main thread in `example/rust/` and confirm the
unbalanced `end_pass`. Requires running the example in a Godot editor — this is not
verifiable headlessly, so plan for a manual run and record what was observed.

If the reproduction turns out to be blocked before it reaches the claim (for instance by
`to_gd()` failing off-thread first), that result decides the design question and should
be written up as such rather than worked around.

### Phase 2: Choose and implement the resolution

Depends on Phase 1. If the outcome is "narrow the contract", the spec entries listed in
frontmatter change meaning and this stops being a pure bug fix — route that through spec
update before implementing.

Fold the `try_dispose` claim leak into whichever resolution lands.
