# `gdext-egui` egui backend implementation for Godot 4

[Example Godot project](./example)

# Usage

In Cargo.toml, under the `[dependencies]` tab

```toml
godot = { git = "https://github.com/godot-rust/gdext", branch = "master" }
gdext-egui = { git = "https://github.com/kang-sw/gdext-egui", branch = "master" }
```

In Rust, write a GodotClass derivative like this:

```rust
#[derive(GodotClass)]
#[class(init, base=Node)]
struct Showcase {
    base: Base<Node>,

    /// This should be set from editor
    #[init(default = OnReady::manual())]
    egui: OnReady<Gd<gdext_egui::EguiBridge>>,
}

#[godot_api]
impl INode for Showcase {
    fn ready(&mut self) {
        self.egui.init(gdext_egui::EguiBridge::new_alloc());

        let mut gd_self = self.to_gd();
        gd_self.add_child(self.egui.clone().upcast());
        self.egui.set_owner(gd_self.upcast());

        // Optional, if you want to viewports truly spawn a native window.
        self.base()
            .get_viewport()
            .unwrap()
            .set_embedding_subwindows(false);
    }

    fn process(&mut self, _d: f64) {
        // `ctx` MUST BE acquired from `current_frame()` method !!!
        let ctx = self.egui.bind().current_frame().clone();
        
        // Then use the ctx like as always we use egui.
        egui::Window::new("HAHA!").show(&ctx, |ui| {
          ui.label("hello, world!");
        });
    }
}
```

# Run Example

Start Godot 4.2.1 at [`example/`](example/) directory, giving first argument [`Showcase.tscn`](example/Showcase.tscn)

# Features

- [x] Window support
  - [x] Basic Viewport
    - [x] Creation / Disposal
    - [x] Mouse Input Handling
      - [ ] In-editor widget
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

# Limitations

- Editor Plugin is still under development.
- Few input bugs.
- IME support is sub-optimal.
