#![doc = include_str!("../README.md")]

use std::{
    collections::BTreeMap,
    mem::take,
    sync::{mpsc, Arc},
    time::Instant,
};

use crossbeam_queue::SegQueue;
use egui::{
    ahash::{HashMap, HashSet},
    epaint,
    mutex::Mutex,
    ViewportId,
};
use godot::{
    engine::{
        self, control::FocusMode, control::LayoutPreset, display_server::CursorShape, CanvasLayer,
        Control, DisplayServer, ICanvasLayer, IControl, ImageTexture, InputEvent, InputEventKey,
        InputEventMouseButton, InputEventMouseMotion, RenderingServer, Texture2D,
    },
    obj::NewAlloc,
    prelude::*,
};
use itertools::multizip;

/* ---------------------------------------------------------------------------------------------- */
/*                                    PRIMARY CONTROLLER BRIDGE                                   */
/* ---------------------------------------------------------------------------------------------- */

///
/// BUG: We're disabling `tool` attribute, as it causes the editor to crash on hot-reload.
#[derive(GodotClass)]
#[class(/*tool,*/ init, base=CanvasLayer, rename=GodotEguiBridge)]
pub struct EguiBridge {
    base: Base<CanvasLayer>,

    started: bool,

    viewports: HashMap<ViewportId, ViewportContext>,
    textures: HashMap<egui::TextureId, TextureDescriptor>,

    #[init(default=OnReady::manual())]
    gd_render_viewport: OnReady<Gd<engine::SubViewport>>,
    #[init(default=OnReady::manual())]
    gd_root_canvas_item: OnReady<Gd<engine::Control>>,

    canvas_items: Vec<Rid>,

    share: SharedContext,

    #[init(default=egui::Rect::NOTHING)]
    cached_screen_rect: egui::Rect,
}

/// Shared among all the viewports.
#[derive(Default, Clone)]
struct SharedContext {
    ctx: egui::Context,
    screen: Arc<Mutex<ScreenBuffer>>,
    txrx_latest_focus_viewport: Arc<Mutex<(ViewportId, bool)>>,
    txrx_events: Arc<SegQueue<egui::Event>>,
    repaint_schedule: Arc<Mutex<HashMap<ViewportId, Instant>>>,
}

#[derive(Default)]
struct ScreenBuffer {
    global_offset: [u32; 2],
    texture: Gd<engine::ViewportTexture>,
}

struct TextureDescriptor {
    gd_src_img: Gd<engine::Image>,
    gd_tex: Gd<ImageTexture>,
}

#[godot_api]
impl ICanvasLayer for EguiBridge {
    fn ready(&mut self) {
        // Instantiate render target viewport, and full stretched canvas item onto it.
        {
            let mut gd_vp = engine::SubViewport::new_alloc();

            self.base_mut().add_child(gd_vp.clone().upcast());
            gd_vp.set_owner(self.to_gd().upcast());

            let mut gd_canvas = engine::Control::new_alloc();
            gd_vp.add_child(gd_canvas.clone().upcast());
            gd_canvas.set_owner(gd_vp.clone().upcast());

            gd_canvas.set_anchors_and_offsets_preset(LayoutPreset::FULL_RECT);

            self.gd_render_viewport.init(gd_vp);
            self.gd_root_canvas_item.init(gd_canvas);
        }

        // Enable egui context viewport support.
        self.share.ctx.set_embed_viewports(false);

        let sched = self.share.repaint_schedule.clone();
        self.share.ctx.set_request_repaint_callback(move |req| {
            let now = Instant::now();

            godot_print!("Requesting Repaint: {:?}", req.viewport_id);

            let mut sched = sched.lock();
            sched.insert(req.viewport_id, now + req.delay);
        });
    }

    fn process(&mut self, delta: f64) {
        self.on_process(delta);
    }

    fn exit_tree(&mut self) {
        if !self.started {
            // Nothing has happened.
            return;
        }

        // Canvas items should manually be disposed.
        let mut rs = RenderingServer::singleton();
        for item in self.canvas_items.drain(..) {
            rs.free_rid(item);
        }

        let _ = self.share.ctx.end_frame();

        // We don't need any specific dispose workflow since every children belongs to
        // this tree, therefore removed with this object.
    }
}

impl EguiBridge {
    pub fn egui_context(&self) -> egui::Context {
        self.share.ctx.clone()
    }

    pub fn on_process(&mut self, delta: f64) {
        // TODO: Allocate render target => total screen minmax boundary
        let gd_ds = DisplayServer::singleton();
        let total_screen_rect = {
            let mut bound = egui::Rect::NOTHING;

            // BUG: On hot-reload, VTable gets broken thus call to `get_screen_count`
            // incurs editor crash.
            for idx_screen in 0..gd_ds.get_screen_count() {
                let min = gd_ds.screen_get_position_ex().screen(idx_screen).done();
                let size = gd_ds.screen_get_size_ex().screen(idx_screen).done();

                let max = min + size;
                bound.extend_with(egui::pos2(min.x as _, min.y as _));
                bound.extend_with(egui::pos2(max.x as _, max.y as _));
            }

            bound
        };

        // 'end_frame()` should be called from second iteration.
        if !self.started {
            self.started = true;

            // Spawn root viewport
            self.spawn_viewport(egui::ViewportId::ROOT, None);
        } else {
            // Check if any of repaint schedule is expired.
            let should_repaint = {
                let sched = self.share.repaint_schedule.lock();
                let now = Instant::now();

                sched.iter().any(|(_, time)| *time < now)
            };

            if !should_repaint {
                return;
            }

            // Refresh cached screen rectangle, before dealing with render target.
            if self.cached_screen_rect != total_screen_rect {
                self.cached_screen_rect = total_screen_rect;

                // Create render target texture with maximum size.
                let size = self.cached_screen_rect.size();

                let mut tex = self.share.screen.lock();
                let min = self.cached_screen_rect.min;

                tex.global_offset = [min.x, min.y].map(|x| x as _);

                // Resize render target texture
                let rt = &mut *self.gd_render_viewport;
                rt.set_size(Vector2i::new(size.x as _, size.y as _));
                tex.texture = rt.get_texture().unwrap_or_default();

                godot_print!("Resizing render target: {:?}", size);

                // XXX: should we deal with `16384 x 16384` screen size limitation?
                // - Hint is utilizing `global_offset`, to actual region that the editor is
                //   using. e.g. Limit this to primary monitor size when it exceeds the limit.
            }

            // From second frame, we start to dealing with screen size
            let full_output = self.share.ctx.end_frame();
            self.handle_output(full_output);
        }

        let (active_viewport, is_focused_any) = {
            // Viewport and focus always guaranteed to be valid.

            let (ref mut vp, ref mut fc) = *self.share.txrx_latest_focus_viewport.lock();
            if !self.viewports.contains_key(vp) {
                *vp = egui::ViewportId::ROOT;
                *fc = false;
            }

            (*vp, *fc)
        };

        let raw = egui::RawInput {
            viewport_id: active_viewport,
            viewports: self
                .viewports
                .iter()
                .map(|(id, value)| (*id, value.input.lock().clone()))
                .collect(),

            screen_rect: Some(total_screen_rect),

            // FIXME: Remove this magic number!
            max_texture_side: Some(8192),

            time: Some(engine::Time::singleton().get_ticks_msec() as f64 / 1e3),
            predicted_dt: delta as _,

            modifiers: {
                use engine::global::Key as GdKey;

                let gd_input = engine::Input::singleton();
                let is_pressed = |k: GdKey| gd_input.is_physical_key_pressed(k);

                egui::Modifiers {
                    alt: is_pressed(GdKey::ALT),
                    ctrl: is_pressed(GdKey::CTRL),
                    shift: is_pressed(GdKey::SHIFT),
                    command: is_pressed(GdKey::META),
                    mac_cmd: is_pressed(GdKey::META),
                }
            },
            events: std::iter::repeat_with(|| self.share.txrx_events.pop())
                .map_while(|x| x)
                .collect(),

            focused: is_focused_any,

            // TODO: deal with these.
            hovered_files: Vec::default(),
            dropped_files: Vec::default(),
        };

        // Start next frame rendering.
        self.share.ctx.begin_frame(raw);

        {
            // FIXME: Remove test code

            egui::debug_text::print(&self.share.ctx, "Debug Text Rendering");

            egui::Window::new("Test Window").show(&self.share.ctx, |ui| {
                ui.label("Hello, World!");
            });
        }

        // Now in any code, draw operation can be performed with `self.ctx` object.
    }

    fn handle_output(&mut self, output: egui::FullOutput) {
        /* -------------------------- Deffered Viewport Rendering Code -------------------------- */
        let gd_ds = DisplayServer::singleton();
        let mut viewport_ids = HashSet::from_iter(self.viewports.keys().copied());
        let now = Instant::now();

        let mut repainted_viewports = Vec::new();

        for (id, vp_output) in output.viewport_output {
            if !viewport_ids.remove(&id) {
                self.spawn_viewport(id, Some((vp_output.parent, vp_output.builder)));
            } else {
                let viewport = self.viewports.get_mut(&id).unwrap();
                let (commands, recreate) = viewport.window_setup.patch(vp_output.builder);

                if recreate {
                    // Respawn viewport
                    let init = take(&mut viewport.window_setup);
                    self.despawn_viewport(id);
                    self.spawn_viewport(id, Some((vp_output.parent, init)));
                } else {
                    viewport.apply_commands(&self.share, commands)
                }
            };

            let repaint = self
                .share
                .repaint_schedule
                .lock()
                .remove(&id)
                .map(|y| (vp_output.viewport_ui_cb, y));

            if let Some((deferred, repaint_at)) = repaint {
                if repaint_at <= now {
                    // It's time to redraw

                    if let Some(cb) = deferred {
                        cb(&self.share.ctx)
                    };

                    repainted_viewports.push(id);
                } else {
                    // Oops, we're too early, let's put it back
                    self.share.repaint_schedule.lock().insert(id, repaint_at);
                }
            }
        }

        for id in viewport_ids {
            // Remove disappeared viewport window.
            self.despawn_viewport(id);
        }

        /* -------------------------------------- Painting -------------------------------------- */

        for (id, src) in output.textures_delta.set {
            godot_print!("Adding Texture: {:?}", id);

            // Retrieve image from delivered data
            let src_image = {
                let mut payload = PackedByteArray::new();
                payload
                    .resize(src.image.bytes_per_pixel() * src.image.width() * src.image.height());

                let format = match &src.image {
                    // SAFETY: Using unsafe to reinterpret slice bufers to byte array.
                    egui::ImageData::Color(x) => unsafe {
                        // We just assume that the image is in RGBA8 format.
                        let as_u8_slice = x.pixels.align_to::<u8>().1;
                        payload.as_mut_slice().copy_from_slice(as_u8_slice);

                        // payload.as_mut_slice().copy;
                        engine::image::Format::RGBA8
                    },
                    egui::ImageData::Font(x) => unsafe {
                        let as_u8_slice = x.pixels.align_to::<u8>().1;
                        payload.as_mut_slice().copy_from_slice(as_u8_slice);

                        engine::image::Format::RF
                    },
                };

                let Some(src_image) = godot::engine::Image::create_from_data(
                    src.image.width() as _,
                    src.image.height() as _,
                    false,
                    format,
                    payload,
                ) else {
                    godot_error!("Failed to create image from data!");
                    continue;
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
                let Some(gd_tex) = engine::ImageTexture::create_from_image(src_image.clone())
                else {
                    godot_error!("Failed to create texture from image!");
                    continue;
                };

                let tex = TextureDescriptor {
                    gd_src_img: src_image,
                    gd_tex,
                };

                // In this case, texture MUST be new.
                assert!(self.textures.insert(id, tex).is_none());
            }
        }

        // TODO: Render all shapes into the render target

        // FIXME: Pixels Per Point handling
        let mut rs = RenderingServer::singleton();

        let pixels_per_point = 1.;
        let primitives = self.share.ctx.tessellate(output.shapes, pixels_per_point);

        // Performs bookkeeping for each tessellated meshes
        self.canvas_items
            .reserve(self.canvas_items.len().min(primitives.len()));
        for draw_index in self.canvas_items.len()..primitives.len().max(self.canvas_items.len()) {
            // TODO: insert new canvas item
            let rid = rs.canvas_item_create();
            self.canvas_items.push(rid);

            rs.canvas_item_set_parent(rid, self.gd_root_canvas_item.get_canvas_item());
            rs.canvas_item_set_clip(rid, true);
            rs.canvas_item_set_draw_index(rid, draw_index as _);
        }

        // Free any unused canvas items
        for rid in self
            .canvas_items
            .drain(primitives.len()..self.canvas_items.len())
        {
            rs.free_rid(rid);
        }

        // Perform painting for each primitives
        for (idx_rid, primitive) in primitives.into_iter().enumerate() {
            match primitive.primitive {
                epaint::Primitive::Mesh(mesh) => {
                    let rid = self.canvas_items[idx_rid];
                    rs.canvas_item_clear(rid);

                    if mesh.is_empty() {
                        // Clear canvas_item BEFORE checking mesh is empty;
                        continue;
                    }

                    // Create mesh from `mesh` data.
                    self.render_mesh(&mut rs, rid, mesh);
                }
                epaint::Primitive::Callback(_) => {
                    // XXX: Is there any way to deal with this?
                    unimplemented!()
                }
            }
        }

        // - Then iterate each control -> let them 'bite' part of render target for
        //   drawing.

        for id in output.textures_delta.free {
            // TODO: Free Textures
            godot_print!("Freeing Texture: {:?}", id);

            // Let it auto-deleted.
            let Some(_) = self.textures.remove(&id) else {
                // Texture could be uninitialized due to error.
                godot_warn!("Texture not found! {:?}", id);
                continue;
            };
        }

        for viewport_id in repainted_viewports {
            let viewport = self.viewports.get(&viewport_id).unwrap();
            let mut control = viewport.control.clone();

            control.queue_redraw();
        }
    }

    fn render_mesh(&mut self, rs: &mut RenderingServer, rid_item: Rid, mesh: egui::Mesh) {
        let Some(texture) = self
            .textures
            .get(&mesh.texture_id)
            .map(|x| x.gd_tex.clone())
        else {
            godot_warn!("Missing Texture: {:?}", mesh.texture_id);
            return;
        };

        // FIXME: Debug Code
        {
            for face in mesh.indices.windows(3) {
                let idxs: [_; 3] = std::array::from_fn(|i| face[i] as usize);
                let v = idxs.map(|i| mesh.vertices[i]);
                let p = v.map(|v| Vector2::new(v.pos.x, v.pos.y));
                let c = v.map(|v| v.color).map(|_| Color::MAGENTA);

                rs.canvas_item_add_line(rid_item, p[0], p[1], c[0]);
                rs.canvas_item_add_line(rid_item, p[1], p[2], c[1]);
                rs.canvas_item_add_line(rid_item, p[2], p[0], c[2]);
            }
        }

        let mut verts = PackedVector2Array::new();
        let mut uvs = PackedVector2Array::new();
        let mut colors = PackedColorArray::new();
        let mut indices = PackedInt32Array::new();

        verts.resize(mesh.vertices.len());
        colors.resize(mesh.vertices.len());
        uvs.resize(mesh.vertices.len());

        indices.resize(mesh.indices.len());

        for (src, d_vert, d_uv, d_color) in itertools::multizip((
            mesh.vertices.as_slice(),
            verts.as_mut_slice(),
            uvs.as_mut_slice(),
            colors.as_mut_slice(),
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

        let mut gd_mesh = engine::ArrayMesh::new_gd();
        let mut arrays = VariantArray::new();

        use engine::mesh::ArrayType as AT;

        arrays.resize(AT::MAX.ord() as _);

        arrays.set(AT::VERTEX.ord() as _, verts.to_variant());
        arrays.set(AT::TEX_UV.ord() as _, uvs.to_variant());
        arrays.set(AT::COLOR.ord() as _, colors.to_variant());
        arrays.set(AT::INDEX.ord() as _, indices.to_variant());

        type AF = engine::mesh::ArrayFormat;

        gd_mesh
            .add_surface_from_arrays_ex(engine::mesh::PrimitiveType::TRIANGLES, arrays)
            .flags(AF::FLAG_USE_2D_VERTICES)
            .done();

        // godot_print!("surface len:{}", gd_mesh.surface_get_array_len(0));

        // self.gd_drawer.add_child(mesh_instance.upcast());

        // self.gd_drawer.draw_mesh(gd_mesh.upcast(), texture.upcast());
        gd_mesh.get_rid();

        // TODO: Create canvas items for each mesh and add them as viewport item.
    }

    fn spawn_viewport(
        &mut self,
        id: ViewportId,
        windowing: Option<(ViewportId, egui::ViewportBuilder)>,
    ) {
        // TODO: Create and insert viewport.

        let mut gd_control = EguiViewportIoBridge::new_alloc();
        let mut vp = ViewportContext {
            control: gd_control.clone(),
            parent_id: None,
            window: None,
            input: Default::default(),
            window_setup: Default::default(),
        };

        gd_control
            .bind_mut()
            .initiate(id, self.share.clone(), vp.input.clone());

        if let Some((parent, window_init)) = windowing {
            // TODO: If we need to create separate window, setup callbacks
            // - Resized => Re-render signal
            // - Close => Forward viewport close event
            let mut window = engine::Window::new_alloc();

            vp.window_setup = window_init;
            vp.parent_id = Some(parent);

            // TODO: Setup initial window configs
            godot_print!("Spawned Window!");

            self.base_mut().add_child(window.clone().upcast());
            window.set_owner(self.to_gd().upcast());

            window.add_child(gd_control.clone().upcast());
            gd_control.set_owner(window.clone().upcast());

            gd_control.set_name(format!("Viewport {:?}", id).to_godot());
        } else {
            godot_print!("Spawned Root!");

            self.base_mut().add_child(gd_control.clone().upcast());
            gd_control.set_owner(self.to_gd().upcast());

            gd_control.set_name("Viewport Root".to_godot());
        }

        self.viewports.insert(id, vp);
    }

    fn despawn_viewport(&mut self, id: ViewportId) {
        let mut context = self.viewports.remove(&id).unwrap();
        context.control.queue_free();

        if let Some(mut window) = context.window {
            // Should be freed AFTER control, since it's parent of it.
            window.queue_free();
        }

        // We don't need to deal with focus validity; as it's corrected automatically on
        // every frame start.
    }
}

/* ------------------------------------------- Context ------------------------------------------ */

struct ViewportContext {
    parent_id: Option<ViewportId>,

    control: Gd<EguiViewportIoBridge>,
    input: Arc<Mutex<egui::ViewportInfo>>,

    window: Option<Gd<engine::Window>>,
    window_setup: egui::ViewportBuilder,
}

impl ViewportContext {
    fn apply_commands(&mut self, share: &SharedContext, commands: Vec<egui::ViewportCommand>) {
        for command in commands {
            use egui::ViewportCommand::*;

            match command {
                Close => (),
                CancelClose => (),
                Title(_) => (),
                Transparent(_) => (),
                Visible(_) => (),
                StartDrag => (),
                OuterPosition(_) => (),
                InnerSize(_) => (),
                MinInnerSize(_) => (),
                MaxInnerSize(_) => (),
                ResizeIncrements(_) => (),
                BeginResize(_) => (),
                Resizable(_) => (),
                EnableButtons {
                    close,
                    minimized,
                    maximize,
                } => (),
                Minimized(_) => (),
                Maximized(_) => (),
                Fullscreen(_) => (),
                Decorations(_) => (),
                WindowLevel(_) => (),
                Icon(_) => (),
                IMERect(_) => (),
                IMEAllowed(_) => (),
                IMEPurpose(_) => (),
                Focus => (),
                RequestUserAttention(_) => (),
                SetTheme(_) => (),
                ContentProtected(_) => (),
                CursorPosition(_) => (),
                CursorGrab(_) => (),
                CursorVisible(_) => (),
                MousePassthrough(_) => (),
                Screenshot => (),
            }
        }
    }
}

/* ------------------------------------------ Windowing ----------------------------------------- */

struct WindowEventItem {
    id: ViewportId,
    ev: egui::ViewportCommand,
}

/* ---------------------------------------------------------------------------------------------- */
/*                                        VIEWPORT CONTROL                                        */
/* ---------------------------------------------------------------------------------------------- */

/// One EGUI Viewport -> One Egui node
#[derive(GodotClass)]
#[class(init, base=Control, hidden, rename=Internal__GodotEguiViewportIoBridge)]
struct EguiViewportIoBridge {
    base: Base<Control>,

    self_id: ViewportId,
    share: SharedContext,
    input: Arc<Mutex<egui::ViewportInfo>>,
}

#[godot_api]
impl IControl for EguiViewportIoBridge {
    fn ready(&mut self) {
        {
            let mut base = self.base_mut();
            base.set_focus_mode(FocusMode::CLICK);
            base.set_mouse_filter(engine::control::MouseFilter::STOP);
            base.set_anchors_and_offsets_preset(LayoutPreset::FULL_RECT);
        }
        // This should be able to accept focus, when clicked.
        let ctx = self.share.ctx.clone();
        let self_id = self.self_id;

        self.base_mut().connect(
            StringName::from_latin1_with_nul(b"resized\0"),
            Callable::from_fn("Resize Observer", move |_vars| {
                ctx.request_repaint_of(self_id);
                Ok(Variant::nil())
            }),
        );
    }

    fn draw(&mut self) {
        // TODO: self.base_mut().draw_texture_rect_region(texture, rect, src_rect);
        // - Draw the render target texture, with the given rectangle.

        let gd_ds = DisplayServer::singleton();
        let texture = self.share.screen.lock().texture.clone().upcast();

        // Bit blit the texture to the screen
        {
            let mut base = self.base_mut();

            let global_offset = gd_ds
                .window_get_position_ex()
                .window_id(base.get_window().map(|x| x.get_window_id()).unwrap_or(-1))
                .done();
            let screen_pos = base.get_screen_position();
            let size = base.get_size();

            let offset = Vector2::new(
                global_offset.x as f32 + screen_pos.x,
                global_offset.y as f32 + screen_pos.y,
            );

            godot_print!("{:?}, {:?}", offset, size);

            base.draw_texture_rect_region(
                texture,
                Rect2::new(Vector2::ZERO, size),
                Rect2::new(offset, size),
            );

            base.draw_line(Vector2::new(0., 0.), Vector2::new(23., 41.), Color::CRIMSON);
        }

        // TODO: target rectangle is [global_offset + screen_pos, size]
    }

    fn gui_input(&mut self, event: Gd<InputEvent>) {
        // TODO: Parse event and convert to EGUI raw input, translating it to viewport offset.

        let mouse_button = event.clone().try_cast::<InputEventMouseButton>().ok();
        let mouse_motion = event.clone().try_cast::<InputEventMouseMotion>().ok();
        let keyboard_event = event.clone().try_cast::<InputEventKey>().ok();

        let event_accepted =
            mouse_button.is_some() || mouse_motion.is_some() || keyboard_event.is_some();

        // if let Some(mouse) = mouse_button {
        //     godot_print!("Caught Mouse Event!");
        // }

        // if let Some(mouse) = mouse_motion {
        //     godot_print!("Caught Mouse Motion Event!");
        // }

        // if let Some(key) = keyboard_event {
        //     godot_print!("Caught Keyboard Event!");
        // }

        // godot_print!("Event!");

        if event_accepted {
            // Request redraw of this viewport.
            self.request_repaint();

            // Consume any input event that was delivered to this control.

            // FIXME: Only accept event when any window hit is detected.
            self.base_mut().accept_event();
        }
    }
}

impl EguiViewportIoBridge {
    pub fn initiate(
        &mut self,
        id: ViewportId,
        share: SharedContext,
        input: Arc<Mutex<egui::ViewportInfo>>,
    ) {
        self.self_id = id;
        self.share = share;
        self.input = input;
    }

    fn request_repaint(&self) {
        self.share.ctx.request_repaint_of(self.self_id);
    }
}
