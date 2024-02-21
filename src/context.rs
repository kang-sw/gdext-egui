use std::{
    sync::{
        atomic::{AtomicBool, AtomicU64},
        mpsc, Arc,
    },
    thread::ThreadId,
    time::Instant,
};

use derive_setters::Setters;
use educe::Educe;
use egui::{
    ahash::HashMap, mutex::Mutex, DeferredViewportUiCallback, FullOutput, ViewportBuilder,
    ViewportClass, ViewportId, ViewportIdMap,
};
use godot::{
    engine::{self, CanvasLayer, Control, ICanvasLayer, WeakRef},
    prelude::*,
};
use tap::prelude::{Pipe, Tap};

use crate::painter;

/* ---------------------------------------------------------------------------------------------- */
/*                                             BRIDGE                                             */
/* ---------------------------------------------------------------------------------------------- */

/// Primary Egui Interface.
#[derive(GodotClass)]
#[class(base=CanvasLayer, init, rename=GodotEguiBridge)]
pub struct EguiBridge {
    base: Base<engine::CanvasLayer>,
    share: Arc<SharedContext>,

    /// Access to this variable should to be main-thread only.
    control_outputs: FullOutput,

    /// Actual Godot Nodes for realization of viewports.
    painters: ViewportIdMap<PainterContext>,
}

struct PainterContext {
    /// Actual painter window.
    painter: Gd<painter::EguiViewportBridge>,

    /// Container window if exist. (All widgets without root)
    window: Option<Gd<engine::Window>>,
}

#[derive(Educe)]
#[educe(Default)]
struct SharedContext {
    egui: egui::Context,

    /// Detects whether to start new frame.
    fence_frame: Arc<Mutex<(u64, u64)>>,

    /// Template input for each viewport rendering.
    raw_input_template: Mutex<egui::RawInput>,

    /// Accumulated output for entire single frame.
    full_output: Mutex<egui::FullOutput>,

    /// List of widgets that is tracked by this context.
    spawned_widgets: Arc<Mutex<Vec<SpawnedWidgetInfo>>>,

    /// List of viewports that is tracked by this context.
    spawned_viewports: Mutex<ViewportIdMap<SpawnedViewportContext>>,

    /// List of viewports that
    viewports: Mutex<ViewportIdMap<ViewportContext>>,

    /// The thread ID that instance was initiated.
    #[educe(Default = std::thread::current().id())]
    main_thread_id: ThreadId,
}

struct SpawnedWidgetInfo {
    build_window: Box<FnBuildWindow>,
    show: Box<FnShowWidget>,
}

struct SpawnedViewportContext {
    /// Captures `dispose`, then set it to false when viewport closed.
    repaint: Arc<DeferredViewportUiCallback>,

    /// Should spawned viewport be closed?
    dispose: Arc<AtomicBool>,

    init: egui::ViewportBuilder,
}

struct ViewportContext {
    /// Repainted when time point reaches here.
    repaint_at: Option<Instant>,

    /// Repaint method, that will be called when ready.
    repaint_with: Option<Arc<DeferredViewportUiCallback>>,

    /// Any input captures from viewport.
    rx_update: mpsc::Receiver<painter::ViewportInput>,

    /// Cached viewport information, that we're currently updating on.
    info: egui::ViewportInfo,
}

/* ------------------------------------------ Godot Api ----------------------------------------- */

#[godot_api]
impl ICanvasLayer for EguiBridge {
    fn ready(&mut self) {
        self.setup_egui()
    }
}

#[godot_api]
impl EguiBridge {
    #[func]
    fn __internal_try_start_frame_inner(&self) {
        self.try_start_frame();
    }

    #[func]
    fn __internal_finish_frame_inner(&mut self) {
        self.finish_frame();
    }
}

/* ------------------------------------- Context Management ------------------------------------- */

type FnBuildWindow = dyn 'static + Send + for<'a> Fn(&'a mut bool) -> egui::Window;
type FnShowWidget = dyn 'static + Send + FnMut(&egui::Ui) -> bool;
type FnShowViewport = dyn FnMut(&egui::Context, ViewportClass) -> bool + 'static + Send;

/// APIs for spawning viewports.
///
/// Key for every APIs are that any access to [`egui::Context`] triggers
impl EguiBridge {
    /// Start a new frame, and return context which you can draw with.
    ///
    /// This is very default way of using EGUI, and anything you draw upon this will be
    /// shown below the spawned root canvas; [`EguiBridge`]
    ///
    /// Use this when you want to draw widget every frame within `process()` function.
    ///
    /// # Panics
    ///
    /// - Called from non main gameplay thread.
    pub fn current_frame(&self) -> &egui::Context {
        self.try_start_frame();

        &self.share.egui
    }

    /// Spawns a widget, which calls delivered draw function on-demand.
    pub fn spawn_main_widget(
        &self,
        window_builder: impl 'static + Send + for<'a> Fn(&'a mut bool) -> egui::Window,
        draw: impl 'static + Send + FnMut(&egui::Ui) -> bool,
    ) {
        // TODO: Append to widget list
    }

    /// Render viewport for current frame.
    ///
    /// This is shortcut to following code.
    ///
    /// ```no_run
    /// # use godot::prelude::*;
    /// # use gdext_egui::*;
    /// # let bridge = EguiBridge::new_alloc();
    /// let id = ViewportId::from_hash("123");
    /// let builder = ViewportBuilder::default();
    ///
    /// bridge.current_frame().show_viewport_immediate(
    ///     id, builder, |ctx, viewport_class| {
    ///         // do something ...    
    ///     }
    /// );
    /// ```
    ///
    /// # Panics
    ///
    ///
    pub fn viewport_current_frame<R>(
        &self,
        id: ViewportId,
        builder: ViewportBuilder,
        show: impl FnOnce(&egui::Context, ViewportClass) -> R,
    ) -> R {
        // TODO: Assert if this is main thread

        self.try_start_frame();
        let egui = &self.share.egui;

        egui.show_viewport_immediate(id, builder, show)
    }

    /// Spawn new viewport, which renders provided callback at the start of next
    /// frame. This is thread-safe, however, you should call exactly once per gameplay
    /// frame to ensure viewport is persisted correctly.
    ///
    /// This is inherently a shortcut to following code.
    ///
    /// ```no_run
    /// # use godot::prelude::*;
    /// # use gdext_egui::*;
    /// # let bridge = EguiBridge::new_alloc();
    /// let id = ViewportId::from_hash("123");
    /// let builder = ViewportBuilder::default();
    ///
    /// bridge.egui_start().show_viewport_deferred(
    ///     id, builder, move |ctx, viewport_class| {
    ///         // do something ...    
    ///
    ///         // Viewport will be retained as long as you
    ///         true
    ///     }
    /// );
    /// ```
    pub fn spawn_viewport(
        &self,
        id: ViewportId,
        builder: ViewportBuilder,
        show: impl FnMut(&egui::Context, ViewportClass) -> bool + 'static + Send,
    ) {
        // TODO: Spawn a viewport which is retained as long as show returns true.

        // Ensure the ui frame gets
        self.queue_try_start_frame();
    }
}

/// Private API implementations
impl EguiBridge {
    fn setup_egui(&mut self) {
        (&self.share.egui).pipe(|ctx| {
            let w_share = Arc::downgrade(&self.share);

            ctx.set_embed_viewports(false);
            ctx.set_request_repaint_callback({
                let w_share = w_share.clone();
                move |repaint| {
                    let Some(share) = w_share.upgrade() else {
                        godot_print!("Repaint requested for disposed egui bridge: {repaint:?}");
                        return;
                    };

                    share.repaint(repaint);
                }
            });
        });
    }

    fn try_start_frame(&self) {
        assert!(std::thread::current().id() == self.share.main_thread_id);

        // Only perform frame start when necessary.
        if !self.share.try_advance_frame_fence() {
            return;
        }

        let Some(mut tree) = self.base().get_tree() else {
            godot_script_error!("Node is not added in tree!");
            return;
        };

        let Some(mut timer) = tree.create_timer(0.001) else {
            godot_error!("Failed to create timer");
            return;
        };

        // End of this frame, `finish_frame` will
        timer.connect(
            "timeout".into(),
            Callable::from_object_method(
                &self.to_gd(),
                string_name!(Self, __internal_finish_frame_inner),
            ),
        );

        // Register immediate renderer for this frame.
        let w_self = utilities::weakref(self.to_gd().to_variant());
        egui::Context::set_immediate_viewport_renderer(move |ctx, viewport| {
            let Ok(this) = w_self.try_to::<Gd<WeakRef>>() else {
                unreachable!();
            };

            let Ok(this) = this.get_ref().try_to::<Gd<Self>>() else {
                // It's just expired.
                return;
            };

            let this = this.bind();

            let p_src = this.share.egui.input(|x| x as *const _);
            let p_new = ctx.input(|x| x as *const _);

            if p_src != p_new {
                // Another EGUI runtime?
                return;
            }

            this.viewport_start_frame(viewport.ids.this);
            (viewport.viewport_ui_cb)(ctx);
            this.viewport_end_frame(viewport.ids.this);
        });

        // Gather global input information
        let share = self.share.clone();
        share
            .viewports
            .lock()
            .pipe(|vp| {
                vp.iter()
                    .map(|(id, value)| (*id, value.info.clone()))
                    .collect::<egui::ViewportIdMap<_>>()
            })
            .pipe(|vp| {
                let mut inputs = share.raw_input_template.lock();
                inputs.viewports = vp;
            });

        // Start root frame as normal.
        self.viewport_start_frame(egui::ViewportId::ROOT);
    }

    fn finish_frame(&mut self) {
        // TODO: Show main frame widgets

        // TODO: End main frame loop.

        // TODO: Validate deferred spawned viewport (won't be shown immediately)

        // TODO: Handle viewport changes from output.

        // TODO: Handle new textures from output.

        // TODO: Show all viewports.

        // TODO: Paint all viewports
    }

    fn viewport_start_frame(&self, id: ViewportId) {
        // NOTE: Seems recursive call to begin_frame is handled by stack internally.

        // TODO: Spawn context if viewport id not exist

        // TODO: Collect inputs from given viewport
    }

    fn viewport_end_frame(&self, id: ViewportId) {
        // TODO: Call UI callback if needed.
    }

    fn paint_viewport(&mut self, id: ViewportId) {
        // TODO:
    }

    /// Start frame in thread-safe manner.
    fn queue_try_start_frame(&self) {
        if std::thread::current().id() == self.share.main_thread_id {
            self.try_start_frame();
            return;
        }

        if !self.share.try_advance_frame_fence() {
            return;
        }

        self.to_gd()
            .call_deferred(string_name!(Self, __internal_try_start_frame_inner), &[]);
    }

    fn advance_frame(&self) {
        let (ref mut current, ref mut requested) = *self.share.fence_frame.lock();
        *requested = *current;
    }
}

impl SharedContext {
    fn repaint(&self, info: egui::RequestRepaintInfo) {
        // TODO: Find viewport context, update repaint timing.
    }

    fn try_advance_frame_fence(&self) -> bool {
        let (ref mut current, ref mut requested) = *self.fence_frame.lock();

        if *current == *requested {
            *requested += 1;
            true
        } else {
            // Current frame is not yet started.
            false
        }
    }
}
