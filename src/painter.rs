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

pub struct ViewportInput {}

/// Represents a spawned viewport
#[derive(GodotClass)]
#[class(base=Control, init, hidden, rename=INTERNAL__GodotEguiViewportBridge)]
pub(crate) struct EguiViewportBridge {
    base: Base<Control>,
}

impl EguiViewportBridge {
    pub fn initiate(&mut self, id: egui::ViewportId, tx: mpsc::Sender<ViewportInput>) {
        todo!()
    }

    pub fn draw(&mut self, textures: &TextureLibrary, shapes: Vec<egui::epaint::ClippedShape>) {
        todo!()
    }
}
