# `gdext-egui` egui backend implementation for Godot 4

[Example Godot project](./example)

# Usage

In Cargo.toml, under the `[dependencies]` tab

```toml
godot = { git = "https://github.com/godot-rust/gdext", branch = "master" }
gdext-egui = { git = "https://github.com/kang-sw/gdext-egui", branch = "master" }
```

> ## NOTE
>
> If any API breakage from `gdext` master branch crate causes compilation error from this crate, you can add below line to `Cargo.toml` to make it compatible with current(2024-02-23 16:07:10) dependency version:
>
> ```toml
> [patch.crates-io]
> godot = { git = "https://github.com/godot-rust/gdext", rev = "6614030150950ffa6bd0311a2b914b86d5b7e9e9" }
> ```

In Rust, write a GodotClass derivative like this:

```rust
#[derive(GodotClass)]
#[class(init, base=Node)]
struct Showcase {
    base: Base<Node>,

    #[init(default = OnReady::manual())]
    egui: OnReady<Gd<gdext_egui::EguiBridge>>,

    demos: egui_demo_lib::DemoWindows,
}

#[godot_api]
impl INode for Showcase {
    fn ready(&mut self) {
        self.egui.init(gdext_egui::EguiBridge::new_alloc());

        // `EguiBridge` MUST be registered in scene tree to work properly!
        let mut gd_self = self.to_gd();
        gd_self.add_child(self.egui.clone().upcast());
        self.egui.set_owner(gd_self.upcast());
    }
    
    fn process(&mut self, _d: f64) {
        // If you hope to put UI code in main loop, you MUST get `egui::Context` 
        // via `EguiBridge::current_frame()` method!
        let ctx = self.egui.bind().current_frame().clone();
        self.demos.ui(&ctx);
    }
}
```

# Unsafety

Bunch of unsafe blocks are used to implement `drag and drop` scheme between egui and godot
native ui. Generally it is considered safe since both of godot native and 

# Features

- [x] Window support
  - [x] Basic Viewport
    - [x] Creation / Disposal
    - [x] Mouse Input Handling
      - [ ] Editor extension
      - [ ] In-editor viewport
    - [ ] Text Input / IME support
  - [ ] "GUEST MODE" Viewports
    - Spawn EGUI layer onto any existing window, other than root viewport.
  - [x] Viewport Close Signal
  - [ ] Drag-n-Drop
    - [ ] <-> Godot Editor
    - [ ] <-> OS File System
- [x] Rendering
  - [x] Clipping
- [ ] Utilities
  - [ ] Expose GdScript API (Inherently, a class wrapper for frequently used methods)
  - [ ] Property display (for editor integration)

# Limitations

- Editor Plugin is still under development.
- IME support is sub-optimal.
