use egui::ahash::HashMap;
use godot::{
    engine::{self, Control, IControl, ImageTexture},
    prelude::*,
};

/* ----------------------------------------- Texture Lib ---------------------------------------- */

#[derive(Default)]
pub struct TextureLibrary {
    textures: HashMap<egui::TextureId, TextureDescriptor>,
}

struct TextureDescriptor {
    gd_src_img: Gd<engine::Image>,
    gd_tex: Gd<ImageTexture>,
}

impl TextureLibrary {
    pub fn update_texture(&mut self, id: egui::TextureId, src: egui::epaint::ImageDelta) {
        godot_print!("Adding Texture: {:?}", id);

        // Retrieve image from delivered data
        let src_image = {
            let mut payload = PackedByteArray::new();
            payload.resize(src.image.bytes_per_pixel() * src.image.width() * src.image.height());

            let format = match &src.image {
                // SAFETY: Using unsafe to reinterpret slice bufers to byte array.
                egui::ImageData::Color(x) => {
                    // We just assume that the image is in RGBA8 format.
                    let dst = payload.as_mut_slice();

                    for (i, color) in dst.chunks_mut(4).zip(x.pixels.iter()) {
                        let color = color.to_srgba_unmultiplied();
                        i.copy_from_slice(&color);
                    }

                    // payload.as_mut_slice().copy;
                    engine::image::Format::RGBA8
                }
                egui::ImageData::Font(x) => {
                    let dst = payload.as_mut_slice();

                    for (i, color) in dst.chunks_mut(4).zip(x.srgba_pixels(None)) {
                        let color = color.to_array();
                        i.copy_from_slice(&color);
                    }

                    engine::image::Format::RGBA8
                }
            };

            let Some(src_image) = godot::engine::Image::create_from_data(
                src.image.width() as _,
                src.image.height() as _,
                false,
                format,
                payload,
            ) else {
                godot_error!("Failed to create image from data!");
                return;
            };

            src_image
        };

        if let Some(pos) = src.pos {
            let tex = self.textures.get_mut(&id).unwrap();

            let src_size = src_image.get_size();

            // Partial update on image
            let dst_pos = Vector2i::new(pos[0] as _, pos[1] as _);

            tex.gd_src_img
                .blit_rect(src_image, Rect2i::new(Vector2i::ZERO, src_size), dst_pos);
        } else {
            let Some(gd_tex) = engine::ImageTexture::create_from_image(src_image.clone()) else {
                godot_error!("Failed to create texture from image!");
                return;
            };

            let tex = TextureDescriptor {
                gd_src_img: src_image,
                gd_tex,
            };

            // Replace or insert new texture.
            self.textures.insert(id, tex);
        }
    }

    pub fn free_texture(&mut self, id: egui::TextureId) {
        godot_print!("Freeing Texture: {:?}", id);

        // NOTE: Textures are all ref-counted.
        if self.textures.remove(&id).is_none() {
            // Texture could be uninitialized due to error.
            godot_warn!("Texture not found! {:?}", id);
        };
    }
}

/* ------------------------------------------ Viewport ------------------------------------------ */

/// Represents a spawned viewport
#[derive(GodotClass)]
#[class(base=Control, init, hidden, rename=INTERNAL__GodotEguiViewportBridge)]
pub(crate) struct EguiViewportBridge {
    base: Base<Control>,

    fwd_event: Option<Box<dyn Fn(egui::Event)>>,
}

#[godot_api]
impl IControl for EguiViewportBridge {
    // TODO: Forward every input events to the egui context.
    // - If required (i.e. EGUI is interested in keyboard/mouse input), consume it.
}

impl EguiViewportBridge {
    pub fn initiate(&mut self, on_event: Box<dyn Fn(egui::Event)>) {
        self.fwd_event = Some(on_event);
    }

    pub fn draw(&mut self, textures: &TextureLibrary, shapes: Vec<egui::epaint::ClippedPrimitive>) {
        // TODO: Draw all meshes
    }
}
