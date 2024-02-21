use std::sync::mpsc;

use godot::{engine::Control, prelude::*};

/* ----------------------------------------- Texture Lib ---------------------------------------- */

pub struct TextureLibrary {}

impl TextureLibrary {
    pub fn update_texture(id: egui::TextureId, delta: egui::epaint::ImageDelta) {
        todo!()
    }

    pub fn free_texture(id: egui::TextureId) {
        todo!()
    }
}

/* ------------------------------------------ Viewport ------------------------------------------ */

/// Represents a spawned viewport
#[derive(GodotClass)]
#[class(base=Control, init, hidden, rename=INTERNAL__GodotEguiViewportBridge)]
pub(crate) struct EguiViewportBridge {
    base: Base<Control>,
}

impl EguiViewportBridge {
    pub fn initiate(&mut self, on_event: Box<dyn Fn(egui::Event)>) {
        // TODO:
    }

    pub fn draw(&mut self, textures: &TextureLibrary, shapes: Vec<egui::epaint::ClippedShape>) {
        // TODO:
    }
}
