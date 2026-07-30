---
title: "Callback merge discards re-entrant registrations, and tie ordering is unstable"
sage-review-design: recommended
spec:
  - 260730-reentrant-registration
  - 260730-render-callback-registration
related-mental-model:
  - widget-callbacks
---

# Callback merge discards re-entrant registrations, and tie ordering is unstable

## Background

`invoke_registered_callbacks` in `src/context/frame.rs` takes the registry out of its
`RefCell` before running callbacks, deliberately, so a callback may register more
callbacks while the pass is open. The merge that puts the two halves back is where it
goes wrong.

**Discarded registrations.** The merge appends-and-sorts only when both halves are
non-empty; otherwise it overwrites the registry with the survivors. So when every
surviving callback disposed itself and a callback registered a replacement during the
pass, the survivor list is empty, the branch overwrites, and the replacement is thrown
away with no error. The clearest instance is a `once()` callback that registers its own
successor — the successor never runs and nothing reports it. This directly contradicts
`ai-docs/spec/widget-callbacks.md`, which states re-entrant registration skips no
entries.

**Unstable tie ordering.** Two independent problems, both contradicting the spec's
"Ties keep their existing relative order":

- Insertion uses `binary_search_by_key`, which returns an unspecified index among equal
  keys, so a new callback can land ahead of an already-registered peer.
- The merge path appends survivors *after* the newly registered entries and then sorts
  stably, which systematically places new callbacks ahead of retained peers at the same
  priority.

They are filed together because they are the same six lines and any rewrite of the merge
has to settle both.

## Decisions

Treat the spec as the statement of intent and the code as the defect. Both spec claims
describe behavior a caller would reasonably rely on, and neither is unreasonable to
implement.

Deliberately untouched: the take-then-restore structure itself, which exists to make
re-entrant registration possible at all and is the correct shape. This ticket fixes the
merge, not the strategy.

## Constraints

A panicking callback currently destroys its whole group — the registry is moved into a
local, so an unwind drops every callback in it while the registry stays empty, and the
game keeps running with those widgets silently gone. That is a real consequence of the
same take-then-restore structure, but the fix is different in kind (unwind safety, not
merge logic) and it is deliberately **not** in this ticket's scope. File it separately if
it is worth addressing.

## Phases

### Phase 1: Make the merge total and the ordering stable

Merge unconditionally rather than branching on emptiness, and use a `partition_point`-style
insert so equal priorities append after existing peers on both paths. Verify the merge
path preserves "retained before newly registered" at equal priority, which is the reading
the spec commits to.

Testing is the hard part, as everywhere in this crate: the registry is private and the
callbacks take an `egui::Context`. Check whether the merge logic can be lifted into a
function testable over a plain vector without a bridge instance — if it can, that is the
test; if it cannot, verify through `example/rust/` in the editor and say so in the Result
rather than implying coverage.

Once the behavior matches, the two spec statements stop being wrong and need no edit —
confirm that rather than assuming it.
