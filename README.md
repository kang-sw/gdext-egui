# `gdext-egui` egui backend implementation for Godot 4



# Usage

In Cargo.toml, under the `[dependencies]` tab

```toml
godot = { git = "https://github.com/godot-rust/gdext", branch = "master" }
gdext-egui = { git = "https://github.com/kang-sw/gdext-egui", branch = "master" }
```

# Run Example

Start Godot 4.2.1 at [`example/`](example/) directory, giving first argument [`Showcase.tscn`](example/Showcase.tscn)

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

