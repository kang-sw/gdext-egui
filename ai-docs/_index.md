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

- `ai-docs/mental-model.md` - component map, rendering pipeline, viewport model
- `ai-docs/mental-model/architecture.md` - key types, rendering/input/threading details
  (stale: still describes flat `context.rs` / `surface.rs` and a removed `_widget.rs`)

## Operational Notes

- Everything runs on the Godot main thread; the background task channel exists but threading was
  removed in `5b52d98`.
- Clip rects use a custom canvas-item shader, not Godot's built-in scissor.
- Known gaps: no IME support, editor plugin integration is WIP, menu bar disabled due to layout
  issues.

## Session Notes

- Workflow bootstrapped: `AGENTS.md` is canonical, `CLAUDE.md` is a shim, ticket directories created.
- No specs or tickets exist yet.
