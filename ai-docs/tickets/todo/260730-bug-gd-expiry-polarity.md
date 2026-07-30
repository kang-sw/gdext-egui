---
title: "CheckExpired for Gd<T> reports expiry with inverted polarity"
sage-review-design: recommended
spec:
  - 260730-expiry-sentinels
related-mental-model:
  - widget-callbacks
---

# CheckExpired for Gd<T> reports expiry with inverted polarity

## Background

`bind()` ties a draw callback's lifetime to an expiry sentinel. Grep `impl CheckExpired`
in `src/context/widget_traits.rs`: every implementation answers "is this dead?" by
negating an aliveness signal, except the one for Godot handles, which returns the
validity check unnegated.

The consequence is the exact inverse of the documented contract. Binding a widget to a
live Godot object disposes it on its first frame, and if a callback somehow survived
past the object being freed, it would then report itself alive. Since binding a widget
to the node that owns it is the intended primary use of `bind()`, this makes that path
unusable rather than merely wrong at the margin.

`bind()` and `CheckExpired` are both public, so the broken behavior is already exposed
to downstream users on the published 0.4.x line.

## Decisions

Fix the polarity rather than redefining the trait around it. The trait's contract is
stated by its method name and by the five sibling implementations; the Godot handle is
the outlier, not the definition. Redefining `expired()` to mean "alive" would invert
five correct implementations to accommodate one wrong one, and would silently break any
downstream `CheckExpired` implementation written against the current name.

Deliberately untouched: the rest of the sentinel set, the decorator chain, and
`WidgetRetain`. This ticket is the polarity only.

## Constraints

A downstream user who noticed the inversion may have compensated by binding a handle
they expect to be freed. That is not a supported reading of the API and the fix is not
gated on it, but it belongs in the changelog entry as a behavior change rather than a
silent correction.

## Phases

### Phase 1: Correct the polarity and cover it with a test

Negate the validity check so a valid instance reports not-expired.

Verification is the open question, not the fix. The crate has no tests and every path
in it touches `Gd<T>`, so a unit test needs a Godot object to exist. Decide between:

- a `#[cfg(test)]` test constructing a bare `RefCounted` through gdext, if that works
  without an initialized engine — check before assuming it does;
- an assertion exercised from `example/rust/`, run manually in the editor;
- no automated test, with a manual verification note.

Prefer the first that actually runs. If none do, say so explicitly in the Result rather
than leaving the ticket implying coverage that does not exist.

Also update the Implementation Gap callout in `ai-docs/spec/widget-callbacks.md` — the
gap is being closed, so the callout goes away rather than getting rewritten.
