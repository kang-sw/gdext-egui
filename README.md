

# Usage

In Cargo.toml, under the `[dependencies]` tab

```toml
godot = { git = "https://github.com/godot-rust/gdext", branch = "master" }
gdext-egui = { git = "https://github.com/kang-sw/gdext-egui", branch = "master" }
```

To primary class `EguiBridge` be correctly registered, in any of your gdextension source
code, `EguiBridge::new_alloc()` should be referred within any reachable code path. After
that, you can freely write `egui` code using `EguiBridge::egui_context()`.

> TODO: Add Usage

# Features

- [ ] Viewport support
- [ ] Rendering
- [ ] Mouse input handling
  - [ ] Drag-n-Drop
    - [ ] <-> Godot
    - [ ] <-> OS
- [ ] Keyboard input handling
- [ ] Utilities
  - [ ] Call from GdScript
  - [ ] Property display (for editor integration)
- [ ] Publish to crates.io (when [`gdext`](https://github.com/godot-rust/gdext) 0.1 is ready)


