#![doc = include_str!("../README.md")]

use std::{
    mem::take,
    sync::{mpsc, Arc},
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
        self, control::FocusMode, display_server::CursorShape, CanvasLayer, Control, DisplayServer,
        ICanvasLayer, IControl, ImageTexture, InputEvent, InputEventKey, InputEventMouseButton,
        InputEventMouseMotion, RenderingServer, Texture2D,
    },
    prelude::*,
};

/* ---------------------------------------------------------------------------------------------- */
/*                                    PRIMARY CONTROLLER BRIDGE                                   */
/* ---------------------------------------------------------------------------------------------- */

#[derive(GodotClass)]
#[class(init, base=CanvasLayer)]
pub struct EguiBridge {
    base: Base<CanvasLayer>,

    started: bool,

    viewports: HashMap<ViewportId, ViewportContext>,
    textures: HashMap<egui::TextureId, TextureDescriptor>,

    share: SharedContext,

    #[init(default = egui::Rect::NOTHING)]
    cached_screen_rect: egui::Rect,
}

/// Shared among all the viewports.
#[derive(Default, Clone)]
struct SharedContext {
    ctx: egui::Context,
    render_target: Arc<Mutex<RenderTargetInfo>>,
    txrx_latest_focus_viewport: Arc<Mutex<(ViewportId, bool)>>,
    txrx_events: Arc<SegQueue<egui::Event>>,
}

#[derive(Default)]
struct RenderTargetInfo {
    global_offset: [u32; 2],
    texture: Gd<ImageTexture>,
}

struct TextureDescriptor {
    size: [u32; 2],
    gd_tex: Gd<Texture2D>,
}

#[godot_api]
impl ICanvasLayer for EguiBridge {
    fn ready(&mut self) {
        self.share.ctx.set_embed_viewports(false);
    }

    fn process(&mut self, delta: f64) {
        self.on_process(delta);
    }

    fn exit_tree(&mut self) {
        if !self.started {
            // Nothing has happened.
            return;
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

            for idx_screen in 0..gd_ds.get_screen_count() {
                let min = gd_ds.screen_get_position_ex().screen(idx_screen).done();
                let size = gd_ds.screen_get_size_ex().screen(idx_screen).done();

                let max = min + size;
                bound.extend_with(egui::pos2(min.x as _, min.y as _));
                bound.extend_with(egui::pos2(max.x as _, max.y as _));
            }

            bound
        };

        if self.cached_screen_rect != total_screen_rect {
            self.cached_screen_rect = total_screen_rect;

            // Create render target texture with maximum size.
            let size = self.cached_screen_rect.size();

            let mut tex = self.share.render_target.lock();
            let min = self.cached_screen_rect.min;

            tex.global_offset = [min.x, min.y].map(|x| x as _);

            // TODO: Create render target texture

            // XXX: should we deal with `16384 x 16384` screen size limitation?
            // - Hint is utilizing `global_offset`, to actual region that the editor is
            //   using. e.g. Limit this to primary monitor size when it exceeds the limit.
        }

        // 'end_frame()` should be called from second iteration.
        if !self.started {
            self.started = true;

            // Spawn root viewport
            self.spawn_viewport(egui::ViewportId::ROOT, None);
        } else {
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

        // Now in any code, draw operation can be performed with `self.ctx` object.
    }

    fn handle_output(&mut self, output: egui::FullOutput) {
        /* -------------------------- Deffered Viewport Rendering Code -------------------------- */
        let gd_ds = DisplayServer::singleton();
        let mut viewport_ids = HashSet::from_iter(self.viewports.keys().copied());

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
                    viewport.apply_commands(commands)
                }
            };

            // Deal with viewport renderings
            if let Some(cb) = vp_output.viewport_ui_cb {
                cb(&self.share.ctx);
            }
        }

        for id in viewport_ids {
            // Remove disappeared viewport window.
            self.despawn_viewport(id);
        }

        /* -------------------------------------- Painting -------------------------------------- */

        for (id, image_delta) in output.textures_delta.set {
            // TODO: Update Textures
        }

        // TODO: Render all shapes into the render target

        // FIXME: Pixels Per Point handling

        let primitives = self.share.ctx.tessellate(output.shapes, 1.);

        if !primitives.is_empty() {
            godot_print!("{}", primitives.len());
        }

        for p in primitives {
            match p.primitive {
                epaint::Primitive::Mesh(mesh) => {
                    // TODO: Render this onto render target
                }
                epaint::Primitive::Callback(_) => {
                    // TODO: How should we deal with 3D callbacks?

                    // 1. Providing region of clip rectangle as render target to callback?
                    // 2. Receiving any camera;

                    unimplemented!()
                }
            }
        }

        // - Then iterate each control -> let them 'bite' part of render target for
        //   drawing.

        for id in output.textures_delta.free {
            // TODO: Free Textures
        }
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
            .initiate(self.share.clone(), vp.input.clone());

        if let Some((parent, window_init)) = windowing.filter(|(x, _)| *x != egui::ViewportId::ROOT)
        {
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

            gd_control.set_anchors_preset(engine::control::LayoutPreset::FULL_RECT);
        } else {
            godot_print!("Spawned Root!");

            self.base_mut().add_child(gd_control.clone().upcast());
            gd_control.set_owner(self.to_gd().upcast());
        }

        self.viewports.insert(id, vp);
    }

    fn despawn_viewport(&mut self, id: ViewportId) {
        let mut context = self.viewports.remove(&id).unwrap();
        context.control.queue_free();

        if let Some(mut window) = context.window {
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
    fn apply_commands(&mut self, commands: Vec<egui::ViewportCommand>) {}
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
#[class(init, base=Control, hidden)]
struct EguiViewportIoBridge {
    base: Base<Control>,
    inner: Option<IoBridgeInner>,

    share: SharedContext,
    input: Arc<Mutex<egui::ViewportInfo>>,
}

struct IoBridgeInner {
    ctx: egui::Context,
    atlas: Texture2D,
}

#[godot_api]
impl IControl for EguiViewportIoBridge {
    fn ready(&mut self) {
        // This should be able to accept focus, when clicked.
        self.base_mut().set_focus_mode(FocusMode::CLICK);
    }

    fn draw(&mut self) {
        // TODO: self.base_mut().draw_texture_rect_region(texture, rect, src_rect);
        // - Draw the render target texture, with the given rectangle.

        let gd_ds = DisplayServer::singleton();
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

        base.draw_line(Vector2::new(0., 0.), Vector2::new(23., 41.), Color::CRIMSON);

        godot_print!("{:?}, {:?}", offset, size);

        // TODO: target rectangle is [global_offset + screen_pos, size]
    }

    fn gui_input(&mut self, event: Gd<InputEvent>) {
        // TODO: Parse event and convert to EGUI raw input, translating it to viewport offset.
        let inner = self.inner.as_mut().unwrap();

        let mouse_button = inner
            .ctx
            .wants_pointer_input()
            .then(|| event.clone().try_cast::<InputEventMouseButton>().ok())
            .and_then(|x| x);

        let mouse_motion = inner
            .ctx
            .wants_pointer_input()
            .then(|| event.clone().try_cast::<InputEventMouseMotion>().ok())
            .and_then(|x| x);

        let keyboard_event = inner
            .ctx
            .wants_keyboard_input()
            .then(|| event.clone().try_cast::<InputEventKey>().ok())
            .and_then(|x| x);

        let event_accepted = mouse_button.is_some() || keyboard_event.is_some();

        if let Some(mouse) = mouse_button {}

        if let Some(mouse) = mouse_motion {}

        if let Some(key) = keyboard_event {}

        if event_accepted {
            // Consume any input event that was delivered to this control.
            self.base_mut().accept_event();
        }
    }
}

impl EguiViewportIoBridge {
    pub fn initiate(&mut self, share: SharedContext, input: Arc<Mutex<egui::ViewportInfo>>) {
        self.share = share;
        self.input = input;
    }
}
