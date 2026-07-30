<!-- Memory policy: prune aggressively as project advances. Completed
     work belongs in git history, not here. Keep only what an AI session
     needs to orient itself and pick up work. If it's derivable from
     code or git log, delete it from this file. -->

# gdext-egui Index

## Summary

`gdext-egui` provides [egui](https://github.com/emilk/egui) bindings for Godot 4 through the
[gdext](https://github.com/godot-rust/gdext) framework, so egui immediate-mode UI can run inside
Godot games and editor plugins. Published on crates.io under MPL-2.0.

## Stack

- Rust 2021, crate `gdext-egui` v0.4.1
- `godot` 0.4, `egui` 0.33
- Support crates: `itertools`, `crossbeam-queue`, `oneshot`, `tap`, `derive_setters`, `educe`,
  `with_drop`, `open`

## Workspace

```
src/            library crate (context/, surface/, widgets.rs, lib.rs)
example/        Godot 4 demo project; example/rust/ is the GDExtension crate
ai-docs/        project memory (see AGENTS.md)
```

`Cargo.toml` declares a workspace that excludes `example/.rust`; the example crate builds
separately from `example/rust/`.

## Build / Test

```sh
cargo build
cargo clippy --all-targets
cargo test
```

Rendering, input, and viewport behavior are only observable inside a running Godot instance.
Open `example/project.godot` in Godot 4 and run `Showcase.tscn` for manual verification; ask the
user to run it when a change touches drawing or input.

## Read Before Editing

Specs describe current caller-visible behavior and are the reliable starting point:

- `ai-docs/spec/bridge-lifecycle.md` - the `GodotEguiBridge` node, frame entry points, threading
- `ai-docs/spec/viewport.md` - root/spawned viewports, window commands, close negotiation
- `ai-docs/spec/widget-callbacks.md` - registered draw callbacks, retain/dispose, decorators
- `ai-docs/spec/input.md` - event routing, consumption, key/modifier mapping, clipboard, DnD
- `ai-docs/spec/rendering.md` - canvas items, clip shader, textures, cursor
- `ai-docs/spec/interop-helpers.md` - egui re-export, geometry conversions, DnD payload

Mental models describe the implicit contracts you must not break while changing that behavior:

- `ai-docs/mental-model.md` - crate graph, the single-thread proof, reading map
- `ai-docs/mental-model/frame-lifecycle.md` - the frame claim, pass window, teardown order
- `ai-docs/mental-model/viewport-lifecycle.md` - validate/start/end triple, the deadlock lock
- `ai-docs/mental-model/widget-callbacks.md` - registry invariants, retain/dispose, sentinels
- `ai-docs/mental-model/surface-rendering.md` - RID pooling, clip shader, texture ordering
- `ai-docs/mental-model/input-routing.md` - routing modes, consumption, coordinate spaces

## Operational Notes

- Everything runs on the Godot main thread; the background task channel exists but threading was
  removed in `5b52d98`.
- Clip rects use a custom canvas-item shader, not Godot's built-in scissor.
- Known gaps: no IME support, editor plugin integration is WIP, menu bar disabled due to layout
  issues.

## Session Notes

- Workflow bootstrapped: `AGENTS.md` is canonical, `CLAUDE.md` is a shim, ticket directories created.
- Spec baseline forged across 6 domains (44 stems); mental models reforged across 5 domains.
  No tickets exist yet.
- Confirmed defects with no ticket, found while forging and verified against source:
  - `Gd<T>` expiry sentinel has inverted polarity (`mental-model/widget-callbacks.md`).
  - `egui::Rect` -> `Rect2` writes the max corner into the size field (`spec/interop-helpers.md`).
  - Off-main-thread frame start double-consumes the frame claim, producing an unbalanced
    `end_pass` (`mental-model/frame-lifecycle.md`).
  - Re-entrant callback registration can be silently discarded by the merge in
    `invoke_registered_callbacks` (`mental-model/widget-callbacks.md`).
  - The spawned-viewport input mode is defeated at `NOTIFICATION_READY`, killing the GUI-input
    and drop paths for all viewports (`mental-model/input-routing.md`). Needs runtime
    confirmation in a Godot instance.
- Spec statements contradicted by source and not yet corrected: `spec/widget-callbacks.md`
  claims stable tie ordering and that re-entrant registration skips no entries;
  `spec/input.md` describes Godot-to-egui drops as working.
