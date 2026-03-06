# gdext-egui Mental Model

## Project Overview

**gdext-egui** is a Rust library that provides [egui](https://github.com/emilk/egui) bindings for [Godot 4](https://godotengine.org/) via the [gdext](https://github.com/godot-rust/gdext) (godot-rust) framework. It allows developers to use egui's immediate-mode GUI system inside Godot games and editor plugins.

- **Crate name**: `gdext-egui` (v0.4.1)
- **License**: MPL-2.0
- **Dependencies**: godot 0.4, egui 0.33

## Architecture

See [architecture.md](mental-model/architecture.md) for detailed module descriptions.

### Core Components

- **`EguiBridge`** (`src/context.rs`) - Primary Godot node (`CanvasLayer`-based, `GodotEguiBridge`). Manages the egui context lifecycle, viewport creation/destruction, input forwarding, rendering pipeline, and widget callbacks.
- **`EguiViewportBridge`** (`src/surface.rs`) - Per-viewport Godot `Control` node that handles input events, rendering via `RenderingServer` canvas items, and clip-rect shader-based scissoring.
- **`TextureLibrary`** (`src/surface.rs`) - Manages egui texture allocation/updates as Godot `ImageTexture` objects.
- **Widget System** (`src/widgets.rs`) - Panel-based widget spawning system with predefined layout groups (Left, Right, Central, BottomLeft, BottomRight) and lifecycle management.
- **Helpers** (`src/lib.rs`) - Type conversion traits (`ToCounterpart`) between Godot and egui math types, plus `DragAndDropVariant` for cross-system DnD.

### Rendering Pipeline

1. egui frame begins via `current_frame()` call
2. User code draws UI using `egui::Context`
3. Frame ends: egui outputs `ClippedPrimitive` meshes
4. `EguiViewportBridge::draw()` converts primitives to Godot canvas items via `RenderingServer`
5. Clip rects are implemented via a custom canvas-item shader (not Godot's built-in scissor)

### Viewport Model

- Root viewport is always present (mapped to the main game window)
- Additional viewports can be spawned as native Godot windows (`show_viewport_deferred`, `show_viewport_immediate`, `viewport_spawn`)
- Each viewport gets its own `EguiViewportBridge` Control node

## Current State (v0.4.1)

- Stable for basic use: windows, input, rendering with clip-rect shader
- Published to crates.io
- Known limitations: no IME support, editor plugin integration is WIP, menu bar disabled due to layout issues

## Recent Work

- v0.4.1: Version bump for publishing
- v0.4.0: Added clip-rect shader for proper scissoring, fixed texture partial updates, shutdown panic fix
- Migrated to egui 0.33 (ImageData::Font variant removed)

## Short-term Context

- CLAUDE.md and ai-docs structure just initialized.
