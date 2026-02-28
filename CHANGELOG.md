# Changelog

All notable changes to this project will be documented in this file.

## [0.4.1] - 2026-03-01

### Fixed

- Fix panic on engine shutdown caused by `Variant` captured in egui's thread-local
  outliving the Godot FFI binding lifetime.

## [0.4.0] - 2026-02-26

First release published to [crates.io](https://crates.io/crates/gdext-egui).

### Changed

- Upgrade to `godot` 0.4 (crates.io release) and `egui` 0.33.

### Fixed

- Fix texture partial update logic causing rendering artifacts.
- Fix clipping glitch in surface rendering.

## [0.2.0] - 2025-01-06

### Added

- Initial public release.
- egui context integration as a Godot `CanvasLayer` node (`EguiBridge`).
- Multi-viewport support with spawned native windows.
- Surface-based rendering through Godot's `RenderingServer`.
- Mouse and keyboard input forwarding.
- Widget spawning API for embedding egui panels into the Godot scene tree.
- Cursor shape synchronization.
- UI scaling support.
- Scroll event handling.
- Root region sync for layout placement.
