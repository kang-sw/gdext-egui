use godot::{engine::Control, prelude::*};

/* ----------------------------------------- Texture Lib ---------------------------------------- */

#[derive(Default)]
pub struct TextureLibrary {}

impl TextureLibrary {
    pub fn update_texture(&mut self, id: egui::TextureId, delta: egui::epaint::ImageDelta) {
        // TODO:
    }

    pub fn free_texture(&mut self, id: egui::TextureId) {
        // TODO:
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

    pub fn draw(&mut self, textures: &TextureLibrary, shapes: Vec<egui::epaint::ClippedPrimitive>) {
        // TODO:
    }
}
