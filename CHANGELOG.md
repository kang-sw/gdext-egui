# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Fixed

- Translucent egui colors no longer render too dark. The canvas shader blended egui's
  premultiplied vertex colors with `blend_mix`, applying alpha twice; it now uses
  `blend_premul_alpha` and premultiplies the texture sample.

## [0.5.0] - 2026-09-30

### Changed

- Upgrade to `godot` 0.5 (requires Rust 1.94+). `egui` stays at 0.33.
- `EguiBridge::max_texture_bits` keeps its public Rust accessors
  (`get_max_texture_bits` / `set_max_texture_bits`) through `#[var(pub)]`, since
  godot-rust 0.5 made `#[var(get, set)]` look for user-defined accessors.

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
