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

- [x] Window support
  - [x] Basic Viewport
    - [x] Creation / Disposal
    - [x] Mouse Input Handling
    - [ ] Text Input / IME support
  - [ ] "GUEST MODE" Viewports
    - Spawn EGUI layer onto any existing window, other than root viewport.
  - [x] Viewport Close Signal
  - [ ] Drag-n-Drop
    - [ ] <-> Godot Editor
    - [ ] <-> OS File System
- [x] Rendering
  - [ ] Clipping
- [ ] Utilities
  - [ ] Expose GdScript API (Inherently, a class wrapper for frequently used methods)
  - [ ] Property display (for editor integration)
