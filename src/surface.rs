use egui::ahash::HashMap;
use godot::{
    engine::{self, Control, IControl, ImageTexture, RenderingServer},
    prelude::*,
};
use itertools::multizip;

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

    fn get(&self, id: &egui::TextureId) -> Option<Gd<ImageTexture>> {
        self.textures.get(id).map(|x| x.gd_tex.clone())
    }
}

/* ------------------------------------------ Viewport ------------------------------------------ */

/// Represents a spawned viewport
#[derive(GodotClass)]
#[class(base=Control, init, hidden, rename=INTERNAL__GodotEguiViewportBridge)]
pub(crate) struct EguiViewportBridge {
    base: Base<Control>,

    /// Any GUI event will be forwarded to this.
    fwd_event: Option<Box<dyn Fn(egui::Event)>>,

    /// Rendered primitives
    canvas_items: Vec<Rid>,
}

impl Drop for EguiViewportBridge {
    fn drop(&mut self) {
        // Do not leak resources
        let mut gd_rs = RenderingServer::singleton();

        for rid in self.canvas_items.drain(..) {
            gd_rs.free_rid(rid);
        }
    }
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
        let mut gd_rs = RenderingServer::singleton();

        // Performs bookkeeping - Make `self.canvas_items` be same length as input shapes.
        {
            let rid_self_canvas = self.base().get_canvas_item();

            // Create missing items.
            for index in 0..shapes.len() {
                if index >= self.canvas_items.len() {
                    let rid = gd_rs.canvas_item_create();
                    self.canvas_items.push(rid);

                    gd_rs.canvas_item_set_parent(rid, rid_self_canvas);
                    gd_rs.canvas_item_set_clip(rid, true);
                    gd_rs.canvas_item_set_draw_index(rid, index as _);
                } else {
                    let rid = self.canvas_items[index];
                    gd_rs.canvas_item_clear(rid);
                }
            }

            // Dispose unused items.
            for rid in self
                .canvas_items
                .drain(shapes.len()..self.canvas_items.len())
            {
                gd_rs.free_rid(rid);
            }
        }

        // Render mesh content

        for (primitive, rid_item) in shapes.into_iter().zip(self.canvas_items.iter().cloned()) {
            let egui::epaint::Primitive::Mesh(mesh) = primitive.primitive else {
                godot_error!("unsupported primitive");
                continue;
            };

            let Some(texture) = textures.get(&mesh.texture_id) else {
                godot_warn!("Missing Texture: {:?}", mesh.texture_id);
                return;
            };

            #[cfg(any())]
            for face in mesh.indices.chunks(3) {
                let idxs: [_; 3] = std::array::from_fn(|i| face[i] as usize);
                let v = idxs.map(|i| mesh.vertices[i]);
                let p = v.map(|v| Vector2::new(v.pos.x, v.pos.y));
                let c = v.map(|v| v.color).map(|_| Color::MAGENTA);

                gd_rs.canvas_item_add_line(rid_item, p[0], p[1], c[0]);
                gd_rs.canvas_item_add_line(rid_item, p[1], p[2], c[1]);
                gd_rs.canvas_item_add_line(rid_item, p[2], p[0], c[2]);
            }

            let mut verts = PackedVector2Array::new();
            let mut uvs = PackedVector2Array::new();
            let mut cologd_rs = PackedColorArray::new();
            let mut indices = PackedInt32Array::new();

            verts.resize(mesh.vertices.len());
            cologd_rs.resize(mesh.vertices.len());
            uvs.resize(mesh.vertices.len());

            indices.resize(mesh.indices.len());

            for (src, d_vert, d_uv, d_color) in itertools::multizip((
                mesh.vertices.as_slice(),
                verts.as_mut_slice(),
                uvs.as_mut_slice(),
                cologd_rs.as_mut_slice(),
            )) {
                d_vert.x = src.pos.x;
                d_vert.y = src.pos.y;

                d_uv.x = src.uv.x;
                d_uv.y = src.uv.y;

                d_color.r = src.color.r() as f32 / 255.0;
                d_color.g = src.color.g() as f32 / 255.0;
                d_color.b = src.color.b() as f32 / 255.0;
                d_color.a = src.color.a() as f32 / 255.0;
            }

            for (src, dst) in multizip((mesh.indices.as_slice(), indices.as_mut_slice())) {
                *dst = *src as i32;
            }

            gd_rs
                .canvas_item_add_triangle_array_ex(rid_item, indices, verts, cologd_rs)
                .texture(texture.get_rid())
                .uvs(uvs)
                .done();
        }
    }
}
