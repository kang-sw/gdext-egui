---
title: "egui::Rect to Rect2 conversion writes the max corner into the size field"
sage-review-design: recommended
spec:
  - 260730-to-counterpart-conversions
related-mental-model:
  - frame-lifecycle
---

# egui::Rect to Rect2 conversion writes the max corner into the size field

## Background

`Rect2::new` takes `(position, size)`. Grep `impl ToCounterpart for egui::Rect` in
`src/lib.rs`: both the counterpart and the alternative conversion pass the rectangle's
maximum corner where the extent belongs. The result is correct only when the minimum
corner sits at the origin.

The reverse direction, `Rect2` to `egui::Rect`, builds from min and size correctly, so a
round trip through the pair does not survive. This is a published public helper on the
0.4.x line.

Inside the crate the defect is latent: the only consumer of this direction reads the
position and ignores the size. That is why nothing visibly misbehaves today, and also
why a fix carries no in-crate regression risk.

## Decisions

Fix the arithmetic in place. There is no argument for keeping the current behavior
behind a flag or a second method — a conversion that produces a wrong rectangle is not
a variant anyone selected.

Deliberately untouched: the other five `ToCounterpart` implementations and the
truncate-toward-zero rule on integer conversions, both of which are correct as
specified.

## Constraints

The size field is the only wrong part; position is already right. A downstream user
reading only `.position` — the same pattern the crate itself uses — sees no change.
A user reading `.size` gets a different value after the fix, so this is a behavior
change for the changelog, not a silent correction.

## Phases

### Phase 1: Fix both conversions and pin them with tests

Compute the extent as max minus min for both the counterpart and the alternative form.

This is the one defect in the current backlog that is plausibly testable without a
running engine, if `Rect2`/`Rect2i` construction and field access work outside an
initialized Godot runtime — verify that assumption first, since it is the whole reason
to prefer a unit test here. If it holds, cover both directions and assert the round
trip, which is the property that actually failed.

Update `ai-docs/spec/interop-helpers.md` to drop the Implementation Gap callout once the
behavior matches the table above it.
