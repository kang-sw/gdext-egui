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

Mental-model docs are **stale** and contradicted by the specs above — do not trust them until
they are reforged:

- `ai-docs/mental-model.md` - describes a `src/widgets.rs` panel system that does not exist
- `ai-docs/mental-model/architecture.md` - describes flat `context.rs` / `surface.rs` and a
  removed `_widget.rs`

## Operational Notes

- Everything runs on the Godot main thread; the background task channel exists but threading was
  removed in `5b52d98`.
- Clip rects use a custom canvas-item shader, not Godot's built-in scissor.
- Known gaps: no IME support, editor plugin integration is WIP, menu bar disabled due to layout
  issues.

## Session Notes

- Workflow bootstrapped: `AGENTS.md` is canonical, `CLAUDE.md` is a shim, ticket directories created.
- Spec baseline forged across 6 domains (37 stems). No tickets exist yet.
- Forging surfaced 12 Implementation Gap callouts across the specs, two of which are outright
  defects with no ticket: the `Gd<T>` expiry sentinel has inverted polarity
  (`widget-callbacks.md`), and `egui::Rect` -> `Rect2` conversion writes the max corner into the
  size field (`interop-helpers.md`).
- Mental models still need reforging (`ws:lead-forge-mental-model`).
