---
title: "Spawned-viewport input mode is defeated at NOTIFICATION_READY, killing the GUI-input and drop paths"
sage-review-design: recommended
spec:
  - 260730-input-routing
  - 260730-godot-drop-into-egui
related-mental-model:
  - input-routing
  - viewport-lifecycle
---

# Spawned-viewport input mode is defeated at NOTIFICATION_READY, killing the GUI-input and drop paths

## Background

Two routing modes are intended: the root surface takes the global input path with an
ignore mouse filter, spawned surfaces take the GUI input path with a pass filter. Grep
`set_process_input` in `src/context/viewport.rs` — the spawned branch disables global
input processing *before* the painter is inserted into the tree.

Godot re-enables global input processing at `NOTIFICATION_READY` for any node that
overrides the input virtual, and gdext always registers it because the trait method is
implemented. The disable is therefore discarded on insertion, and spawned painters run
the global path too. That path's else-branch unconditionally sets the mouse filter to
ignore on every event, and Godot skips ignored controls when searching for GUI-input and
drop targets.

The chain ends with the GUI-input path, the drop hooks, and mouse enter/exit
notifications being dead for **every** viewport rather than only the root. Input still
reaches egui through the global path, so the symptom is not "spawned windows do not
respond" — it is that drag-and-drop into egui never fires and hover state sticks when the
pointer leaves.

The same pre-insertion window is where the painter's context and event sink are wired,
deliberately, so this is not a stray ordering mistake — it is one ordering choice serving
two purposes, only one of which it can serve.

## Constraints

**This has not been observed at runtime.** It is derived from reading the engine's node
notification handling and the binding's virtual registration. Every step is checkable in
source, but the conclusion is a chain of four, and a chain that long deserves a
reproduction before anyone writes a fix. Do not treat the Background section as
established behavior until Phase 1 says so.

The existing `TODO: Merge IGNORE behavior between non-root and root` comment in the same
function is the pre-existing acknowledgement that the two modes were never fully
reconciled, and it is the stated prerequisite for `viewport_spawn_as_child` and
`attach_node_to_viewport`. Whatever lands here shapes those.

## Phases

### Phase 1: Confirm or refute the chain

Run `example/Showcase.tscn` with a spawned viewport and check, in order: whether the
spawned painter's global input hook fires at all; whether its mouse filter is ignore
after the first event; whether the GUI-input hook and the drop hooks ever run.

Requires a Godot editor — none of this is verifiable headlessly. Record what was
actually observed, including the case where the chain breaks earlier than predicted,
which would be the more useful result.

### Phase 2: Decide which mode spawned viewports should actually use

Depends on Phase 1 and is deliberately left open, because the answer is not obvious.

If both viewport kinds end up on the global path in practice, the honest resolution may
be to make that the single documented mode and delete the GUI-input branch, rather than
to repair a second path nobody exercises. That would change what
`ai-docs/spec/input.md` describes, so it is a spec decision and not a fix to make
quietly.

If instead the GUI path is worth keeping for spawned windows, the disable has to move to
somewhere that survives insertion, and the global path's unconditional filter reset has
to stop applying to surfaces that are not the root.

Either way the dead `if false & ...` branch guarding root drop support gets resolved
here rather than left in place, and the outcome determines whether the
`260730-godot-drop-into-egui` spec entry is describing something real.
