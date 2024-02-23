use egui::ahash::HashMap;
use godot::{
    engine::{
        self,
        control::{FocusMode, LayoutPreset},
        global::{self, KeyModifierMask},
        notify::ControlNotification,
        Control, DisplayServer, IControl, ImageTexture, InputEventKey, InputEventMouseButton,
        InputEventMouseMotion, RenderingServer,
    },
    prelude::*,
};
use itertools::multizip;
use tap::prelude::Tap;

use crate::helpers::ToCounterpart;

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
#[class(tool, base=Control, init, hidden, rename=INTERNAL__GodotEguiViewportBridge)]
pub(crate) struct EguiViewportBridge {
    base: Base<Control>,

    /// Any GUI event will be forwarded to this.
    fwd_event: Option<Box<dyn Fn(egui::Event)>>,

    /// EGUI context that this viewport is associated with.
    context: Option<egui::Context>,

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
    fn ready(&mut self) {
        self.base_mut().tap_mut(|b| {
            // Makes node to fill the whole available space
            b.set_anchors_and_offsets_preset(LayoutPreset::FULL_RECT);

            // Make this node to be focusable
            b.set_focus_mode(FocusMode::CLICK);
        });
    }

    fn on_notification(&mut self, what: ControlNotification) {
        match what {
            ControlNotification::FocusEnter => {
                self.on_event(egui::Event::WindowFocused(true));
            }
            ControlNotification::FocusExit => {
                self.on_event(egui::Event::WindowFocused(false));
            }
            ControlNotification::MouseExit => {
                self.on_event(egui::Event::PointerGone);
            }
            _ => (),
        }
    }

    fn input(&mut self, event: Gd<engine::InputEvent>) {
        if self.try_consume_input(event) {
            self.mark_input_handled();
        }
    }

    fn gui_input(&mut self, event: Gd<engine::InputEvent>) {
        if self.try_consume_input(event) {
            self.base_mut().accept_event();
        }
    }
}

impl EguiViewportBridge {
    fn on_event(&self, event: egui::Event) {
        if let Some(ev) = self.fwd_event.as_deref() {
            ev(event);
        }
    }

    fn mark_input_handled(&mut self) {
        if let Some(mut vp) = self.base().get_viewport() {
            vp.set_input_as_handled();
        }
    }

    pub fn initiate(&mut self, ctx: egui::Context, on_event: Box<dyn Fn(egui::Event)>) {
        self.context = Some(ctx);
        self.fwd_event = Some(on_event);
    }

    /// Try to consume the input once, and returns whether the input was consumed or not.
    ///
    /// NOTE: This was separated from virtual `input` method, to make `tool` input
    /// handling available.
    pub fn try_consume_input(&mut self, event: Gd<engine::InputEvent>) -> bool {
        let Some(ctx) = &self.context else {
            return false;
        };

        let event = match event.try_cast::<InputEventMouseMotion>() {
            Err(event) => event,
            Ok(event) => {
                self.on_event(egui::Event::PointerMoved(
                    event.get_position().to_alternative(),
                ));

                return ctx.wants_pointer_input();
            }
        };

        let event = match event.try_cast::<InputEventMouseButton>() {
            Err(event) => event,
            Ok(event) => {
                if event.is_canceled() {
                    return false;
                }

                let modifiers = modifier_to_egui(event.get_modifiers_mask());
                let button = event.get_button_index();
                let pos = event.get_position().to_alternative();

                enum Type {
                    Btn(egui::PointerButton),
                    Wheel { horizontal: bool, delta: f32 },
                }

                use Type::*;

                let factor = event.get_factor().tap_mut(|val| {
                    if *val == 0.0 {
                        *val = 4.0;
                    }
                });

                let r = match button {
                    global::MouseButton::LEFT => Btn(egui::PointerButton::Primary),
                    global::MouseButton::RIGHT => Btn(egui::PointerButton::Secondary),
                    global::MouseButton::MIDDLE => Btn(egui::PointerButton::Middle),
                    global::MouseButton::XBUTTON1 => Btn(egui::PointerButton::Extra1),
                    global::MouseButton::XBUTTON2 => Btn(egui::PointerButton::Extra2),

                    global::MouseButton::WHEEL_DOWN => Wheel {
                        horizontal: false,
                        delta: factor,
                    },
                    global::MouseButton::WHEEL_UP => Wheel {
                        horizontal: false,
                        delta: -factor,
                    },
                    global::MouseButton::WHEEL_RIGHT => Wheel {
                        horizontal: true,
                        delta: factor,
                    },
                    global::MouseButton::WHEEL_LEFT => Wheel {
                        horizontal: true,
                        delta: -factor,
                    },
                    _ => return false,
                };

                let event = match r {
                    Btn(button) => egui::Event::PointerButton {
                        pos,
                        button,
                        pressed: event.is_pressed(),
                        modifiers,
                    },
                    Wheel { horizontal, delta } => egui::Event::MouseWheel {
                        unit: egui::MouseWheelUnit::Line,
                        delta: egui::vec2(
                            if horizontal { delta } else { 0.0 },
                            if !horizontal { delta } else { 0.0 },
                        ),
                        modifiers,
                    },
                };

                self.on_event(event);

                return if ctx.wants_pointer_input() {
                    // We grab focus only with clicks
                    self.base_mut().grab_focus();
                    true
                } else {
                    false
                };
            }
        };

        let event = match event.try_cast::<InputEventKey>() {
            Err(event) => event,
            Ok(event) => {
                let key = event.get_keycode();
                let modifiers = modifier_to_egui(event.get_modifiers_mask());

                // Handle copy / cut / paste ...
                if modifiers.matches_logically(egui::Modifiers::CTRL) {
                    let event = match key {
                        global::Key::C => Some(egui::Event::Copy),
                        global::Key::X => Some(egui::Event::Cut),
                        global::Key::V => Some(egui::Event::Paste(
                            DisplayServer::singleton().clipboard_get().into(),
                        )),
                        _ => None,
                    };

                    if let Some(event) = event {
                        self.on_event(event);
                        return ctx.wants_keyboard_input();
                    }
                }

                // @See
                // https://github.com/godotengine/godot/blob/16d61427cab3a8e43f0a9a8ee724fc176b6433c6/scene/gui/text_edit.cpp#L2267
                let unicode = event.get_unicode();

                if event.is_pressed() && unicode >= 32 {
                    let ch = std::char::from_u32(unicode as u32).unwrap();
                    self.on_event(egui::Event::Text(ch.to_string()));
                }

                // if key == global::Key::

                let event = key_to_egui(key).map(|key| egui::Event::Key {
                    key,
                    physical_key: key_to_egui(dbg!(event.get_keycode())),
                    pressed: event.is_pressed(),
                    repeat: event.is_echo(),
                    modifiers,
                });

                if let Some(event) = dbg!(event) {
                    self.on_event(event);
                }

                return ctx.wants_keyboard_input();
            }
        };

        // Every event catch was exhausted. Simply ignore it!
        let _ = event;

        false
    }

    pub fn draw(&mut self, textures: &TextureLibrary, shapes: Vec<egui::epaint::ClippedPrimitive>) {
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
                    gd_rs.canvas_item_set_draw_index(rid, index as i32);
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

            let clip = primitive.clip_rect;
            gd_rs.canvas_item_set_clip(rid_item, true);
            gd_rs
                .canvas_item_set_custom_rect_ex(rid_item, true)
                .rect(clip.to_counterpart())
                .done();

            gd_rs
                .canvas_item_add_triangle_array_ex(rid_item, indices, verts, cologd_rs)
                .texture(texture.get_rid())
                .uvs(uvs)
                .done();
        }
    }
}

fn modifier_to_egui(modifier: KeyModifierMask) -> egui::Modifiers {
    let mut out = egui::Modifiers::default();

    let has = |x: KeyModifierMask| modifier.ord() & x.ord() != 0;

    out.shift = has(KeyModifierMask::SHIFT);
    out.ctrl = has(KeyModifierMask::CTRL | KeyModifierMask::CMD_OR_CTRL);
    out.command = has(KeyModifierMask::CTRL | KeyModifierMask::CMD_OR_CTRL);
    out.mac_cmd = has(KeyModifierMask::CMD_OR_CTRL);
    out.alt = has(KeyModifierMask::ALT);

    out
}

fn key_to_egui(key: global::Key) -> Option<egui::Key> {
    match key {
        // global::Key::NONE => egui::Key::None,
        // global::Key::SPECIAL => egui::Key::Special,
        global::Key::ESCAPE => egui::Key::Escape,
        global::Key::TAB => egui::Key::Tab,
        // global::Key::BACKTAB => egui::Key::Backtab,
        global::Key::BACKSPACE => egui::Key::Backspace,
        global::Key::ENTER => egui::Key::Enter,
        global::Key::KP_ENTER => egui::Key::Enter,
        global::Key::INSERT => egui::Key::Insert,
        global::Key::DELETE => egui::Key::Delete,
        // global::Key::PAUSE => egui::Key::Pause,
        // global::Key::PRINT => egui::Key::Print,
        // global::Key::SYSREQ => egui::Key::Sysreq,
        // global::Key::CLEAR => egui::Key::Clear,
        global::Key::HOME => egui::Key::Home,
        global::Key::END => egui::Key::End,
        global::Key::LEFT => egui::Key::ArrowLeft,
        global::Key::UP => egui::Key::ArrowUp,
        global::Key::RIGHT => egui::Key::ArrowRight,
        global::Key::DOWN => egui::Key::ArrowDown,
        global::Key::PAGEUP => egui::Key::PageUp,
        global::Key::PAGEDOWN => egui::Key::PageDown,
        // global::Key::SHIFT => egui::Key::Shift,
        // global::Key::CTRL => egui::Key::Ctrl,
        // global::Key::META => egui::Key::Meta,
        // global::Key::ALT => egui::Key::Alt,
        // global::Key::CAPSLOCK => egui::Key::Capslock,
        // global::Key::NUMLOCK => egui::Key::Numlo,
        // global::Key::SCROLLLOCK => egui::Key::Scrol,
        global::Key::F1 => egui::Key::F1,
        global::Key::F2 => egui::Key::F2,
        global::Key::F3 => egui::Key::F3,
        global::Key::F4 => egui::Key::F4,
        global::Key::F5 => egui::Key::F5,
        global::Key::F6 => egui::Key::F6,
        global::Key::F7 => egui::Key::F7,
        global::Key::F8 => egui::Key::F8,
        global::Key::F9 => egui::Key::F9,
        global::Key::F10 => egui::Key::F10,
        global::Key::F11 => egui::Key::F11,
        global::Key::F12 => egui::Key::F12,
        global::Key::F13 => egui::Key::F13,
        global::Key::F14 => egui::Key::F14,
        global::Key::F15 => egui::Key::F15,
        global::Key::F16 => egui::Key::F16,
        global::Key::F17 => egui::Key::F17,
        global::Key::F18 => egui::Key::F18,
        global::Key::F19 => egui::Key::F19,
        global::Key::F20 => egui::Key::F20,
        // global::Key::F21 => egui::Key::F21,
        // global::Key::F22 => egui::Key::F22,
        // global::Key::F23 => egui::Key::F23,
        // global::Key::F24 => egui::Key::F24,
        // global::Key::F25 => egui::Key::F25,
        // global::Key::F26 => egui::Key::F26,
        // global::Key::F27 => egui::Key::F27,
        // global::Key::F28 => egui::Key::F28,
        // global::Key::F29 => egui::Key::F29,
        // global::Key::F30 => egui::Key::F30,
        // global::Key::F31 => egui::Key::F31,
        // global::Key::F32 => egui::Key::F32,
        // global::Key::F33 => egui::Key::F33,
        // global::Key::F34 => egui::Key::F34,
        // global::Key::F35 => egui::Key::F35,
        // global::Key::KP_MULTIPLY => egui::Key::KpMultiply,
        // global::Key::KP_DIVIDE => egui::Key::KpDivide,
        // global::Key::KP_SUBTRACT => egui::Key::KpSubtract,
        // global::Key::KP_PERIOD => egui::Key::KpPeriod,
        // global::Key::KP_ADD => egui::Key::KpAdd,
        global::Key::KP_0 => egui::Key::Num0,
        global::Key::KP_1 => egui::Key::Num1,
        global::Key::KP_2 => egui::Key::Num2,
        global::Key::KP_3 => egui::Key::Num3,
        global::Key::KP_4 => egui::Key::Num4,
        global::Key::KP_5 => egui::Key::Num5,
        global::Key::KP_6 => egui::Key::Num6,
        global::Key::KP_7 => egui::Key::Num7,
        global::Key::KP_8 => egui::Key::Num8,
        global::Key::KP_9 => egui::Key::Num9,
        // global::Key::MENU => egui::Key::Menu,
        // global::Key::HYPER => egui::Key::Hyper,
        // global::Key::HELP => egui::Key::Help,
        // global::Key::BACK => egui::Key::Back,
        // global::Key::FORWARD => egui::Key::Forward,
        // global::Key::STOP => egui::Key::Stop,
        // global::Key::REFRESH => egui::Key::Refresh,
        // global::Key::VOLUMEDOWN => egui::Key::Volumedown,
        // global::Key::VOLUMEMUTE => egui::Key::Volumemute,
        // global::Key::VOLUMEUP => egui::Key::Volumeup,
        // global::Key::MEDIAPLAY => egui::Key::Mediaplay,
        // global::Key::MEDIASTOP => egui::Key::Mediastop,
        // global::Key::MEDIAPREVIOUS => egui::Key::Mediaprevious,
        // global::Key::MEDIANEXT => egui::Key::Medianext,
        // global::Key::MEDIARECORD => egui::Key::Mediarecord,
        // global::Key::HOMEPAGE => egui::Key::Homepage,
        // global::Key::FAVORITES => egui::Key::Favorites,
        // global::Key::SEARCH => egui::Key::Search,
        // global::Key::STANDBY => egui::Key::Standby,
        // global::Key::OPENURL => egui::Key::Openurl,
        // global::Key::LAUNCHMAIL => egui::Key::Launchmail,
        // global::Key::LAUNCHMEDIA => egui::Key::Launchmedia,
        // global::Key::LAUNCH0 => egui::Key::Launch0,
        // global::Key::LAUNCH1 => egui::Key::Launch1,
        // global::Key::LAUNCH2 => egui::Key::Launch2,
        // global::Key::LAUNCH3 => egui::Key::Launch3,
        // global::Key::LAUNCH4 => egui::Key::Launch4,
        // global::Key::LAUNCH5 => egui::Key::Launch5,
        // global::Key::LAUNCH6 => egui::Key::Launch6,
        // global::Key::LAUNCH7 => egui::Key::Launch7,
        // global::Key::LAUNCH8 => egui::Key::Launch8,
        // global::Key::LAUNCH9 => egui::Key::Launch9,
        // global::Key::LAUNCHA => egui::Key::Launcha,
        // global::Key::LAUNCHB => egui::Key::Launchb,
        // global::Key::LAUNCHC => egui::Key::Launchc,
        // global::Key::LAUNCHD => egui::Key::Launchd,
        // global::Key::LAUNCHE => egui::Key::Launche,
        // global::Key::LAUNCHF => egui::Key::Launchf,
        // global::Key::GLOBE => egui::Key::Globe,
        // global::Key::KEYBOARD => egui::Key::Keyboard,
        // global::Key::JIS_EISU => egui::Key::JisEisu,
        // global::Key::JIS_KANA => egui::Key::JisKana,
        // global::Key::UNKNOWN => egui::Key::Unknown,
        global::Key::SPACE => egui::Key::Space,
        // global::Key::EXCLAM => egui::Key::Exclam,
        // global::Key::QUOTEDBL => egui::Key::Quotedbl,
        // global::Key::NUMBERSIGN => egui::Key::Numbersign,
        // global::Key::DOLLAR => egui::Key::Dollar,
        // global::Key::PERCENT => egui::Key::Percent,
        // global::Key::AMPERSAND => egui::Key::Ampersand,
        // global::Key::APOSTROPHE => egui::Key::Apostrophe,
        // global::Key::PARENLEFT => egui::Key::Parenleft,
        // global::Key::PARENRIGHT => egui::Key::Parenright,
        // global::Key::ASTERISK => egui::Key::Asterisk,
        global::Key::PLUS => egui::Key::Plus,
        global::Key::COMMA => egui::Key::Comma,
        global::Key::MINUS => egui::Key::Minus,
        global::Key::PERIOD => egui::Key::Period,
        global::Key::SLASH => egui::Key::Slash,
        global::Key::KEY_0 => egui::Key::Num0,
        global::Key::KEY_1 => egui::Key::Num1,
        global::Key::KEY_2 => egui::Key::Num2,
        global::Key::KEY_3 => egui::Key::Num3,
        global::Key::KEY_4 => egui::Key::Num4,
        global::Key::KEY_5 => egui::Key::Num5,
        global::Key::KEY_6 => egui::Key::Num6,
        global::Key::KEY_7 => egui::Key::Num7,
        global::Key::KEY_8 => egui::Key::Num8,
        global::Key::KEY_9 => egui::Key::Num9,
        global::Key::COLON => egui::Key::Colon,
        global::Key::SEMICOLON => egui::Key::Semicolon,
        // global::Key::LESS => egui::Key::Less,
        // global::Key::EQUAL => egui::Key::Equal,
        // global::Key::GREATER => egui::Key::Greater,
        // global::Key::QUESTION => egui::Key::Question,
        // global::Key::AT => egui::Key::At,
        global::Key::A => egui::Key::A,
        global::Key::B => egui::Key::B,
        global::Key::C => egui::Key::C,
        global::Key::D => egui::Key::D,
        global::Key::E => egui::Key::E,
        global::Key::F => egui::Key::F,
        global::Key::G => egui::Key::G,
        global::Key::H => egui::Key::H,
        global::Key::I => egui::Key::I,
        global::Key::J => egui::Key::J,
        global::Key::K => egui::Key::K,
        global::Key::L => egui::Key::L,
        global::Key::M => egui::Key::M,
        global::Key::N => egui::Key::N,
        global::Key::O => egui::Key::O,
        global::Key::P => egui::Key::P,
        global::Key::Q => egui::Key::Q,
        global::Key::R => egui::Key::R,
        global::Key::S => egui::Key::S,
        global::Key::T => egui::Key::T,
        global::Key::U => egui::Key::U,
        global::Key::V => egui::Key::V,
        global::Key::W => egui::Key::W,
        global::Key::X => egui::Key::X,
        global::Key::Y => egui::Key::Y,
        global::Key::Z => egui::Key::Z,
        global::Key::BRACKETLEFT => egui::Key::OpenBracket,
        global::Key::BACKSLASH => egui::Key::Backslash,
        global::Key::BRACKETRIGHT => egui::Key::CloseBracket,
        // global::Key::ASCIICIRCUM => egui::Key::Asciicircum,
        // global::Key::UNDERSCORE => egui::Key::Underscore,
        // global::Key::QUOTELEFT => egui::Key::Quoteleft,
        global::Key::BRACELEFT => egui::Key::OpenBracket,
        // global::Key::BAR => egui::Key::Bar,
        global::Key::BRACERIGHT => egui::Key::CloseBracket,
        // global::Key::ASCIITILDE => egui::Key::Asciitilde,
        // global::Key::YEN => egui::Key::Yen,
        // global::Key::SECTION => egui::Key::Section,
        _ => return None,
    }
    .into()
}
