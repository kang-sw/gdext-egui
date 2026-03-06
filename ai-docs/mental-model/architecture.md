# Architecture Details

## Module Map

```
src/
  lib.rs          - Crate root: re-exports, helpers module, DragAndDropVariant, ToCounterpart trait
  context.rs      - EguiBridge (GodotClass, CanvasLayer): main entry point, egui context owner
  surface.rs      - EguiViewportBridge (Control): per-viewport rendering + input; TextureLibrary
  widgets.rs      - SpawnedWidgetContext: panel-based widget system with layout groups
  _widget.rs      - (Empty / placeholder file)
```

## Key Types

### EguiBridge (`context.rs`)

- `#[class(base=CanvasLayer, tool, init, rename=GodotEguiBridge)]`
- Owns: `Arc<SharedContext>`, `TextureLibrary`, viewport surfaces, widget callbacks
- Public API:
  - `current_frame() -> &egui::Context` - starts a new egui frame, must be called each process tick
  - `viewport_spawn()` - create a deferred viewport with callback
  - `setup_context()` - deferred context configuration (e.g. zoom factor)
  - `sync_root_region()` - sync root viewport region to a Control node (for editor plugins)
  - `register_render_callback_first/last()` - register widget render callbacks with priority

### EguiViewportBridge (`surface.rs`)

- `#[class(base=Control, tool, init, internal, rename=INTERNAL__GodotEguiViewportBridge)]`
- Handles all input: mouse motion, buttons, wheel, keyboard, focus, DnD
- Rendering: creates canvas items via RenderingServer, uses clip-rect shader for scissoring
- Input consumption: checks `ctx.wants_pointer_input()` / `ctx.wants_keyboard_input()`

### Widget System (`widgets.rs`)

- `SpawnedWidgetContext` - manages panels with `BTreeMap<(PanelGroup, i32), PanelItem>`
- `PanelGroup` enum: Left, Right, Central, BottomLeft, BottomRight
- `FnEguiDrawExt` trait: composable widget lifecycle decorators (`.bind()`, `.lifespan()`, `.once()`, `.expires_at()`)
- `WidgetRetain` enum: Retain or Dispose (returned by widget draw callbacks)

### Type Conversions (`lib.rs`)

- `ToCounterpart` trait: bidirectional conversion between:
  - `Vector2` <-> `egui::Vec2` / `egui::Pos2`
  - `Vector2i` <-> `egui::Vec2` / `egui::Pos2`
  - `Rect2` <-> `egui::Rect`
  - `egui::Rect` <-> `Rect2` / `Rect2i`

## Rendering Details

- Uses Godot `RenderingServer` directly (not scene-tree based rendering)
- Each `ClippedPrimitive` gets its own canvas item
- Clip rects via custom shader (`CLIP_SHADER_CODE` in surface.rs) - fragments outside rect are discarded
- Textures: egui textures mapped to Godot `ImageTexture`, supports partial updates via `blit_rect`

## Input Handling

- Root viewport uses `input()` (global input capture)
- Sub-viewports use `gui_input()` (standard Godot GUI input)
- Key mapping: `key_to_egui()` and `modifier_to_egui()` in surface.rs
- Clipboard: Ctrl+C/X/V handled manually via `DisplayServer::clipboard_get()`

## Threading Model

- Single-threaded: `_non_send_sync: PhantomData<*const ()>` enforces this
- `DragAndDropVariant` uses unsafe Send+Sync on Variant (safe in practice since all on main thread)
- Background task channel exists (`tx_bg_task`/`rx_bg_task`) but threading was removed (commit `5b52d98`)
