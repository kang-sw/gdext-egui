use std::{
    sync::{atomic::AtomicU64, Arc},
    thread::ThreadId,
};

use derive_new::new;
use derive_setters::Setters;
use egui::{ahash::HashMap, mutex::Mutex, ViewportBuilder, ViewportClass, ViewportId};
use godot::{
    engine::{self, CanvasLayer, ICanvasLayer, WeakRef},
    prelude::*,
};
use tap::prelude::{Pipe, Tap};

/* ---------------------------------------------------------------------------------------------- */
/*                                             BRIDGE                                             */
/* ---------------------------------------------------------------------------------------------- */

/// Primary Egui Interface.
#[derive(GodotClass)]
#[class(base=CanvasLayer, init, rename=GodotEguiBridge)]
pub struct EguiBridge {
    base: Base<engine::CanvasLayer>,

    #[init(default = Arc::new(Context::new()))]
    share: Arc<Context>,
}

#[derive(new)]
struct Context {
    #[new(default)]
    egui: egui::Context,
    #[new(default)]
    viewports: Arc<Mutex<HashMap<ViewportId, ViewportContext>>>,
    #[new(default)]
    fence_frame: Arc<Mutex<(u64, u64)>>,

    #[new(value = "std::thread::current().id()")]
    main_thread_id: ThreadId,
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
    fn __internal_finish_frame_inner(&self) {
        self.finish_frame();
    }
}

/* ------------------------------------- Context Management ------------------------------------- */

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

            egui::Context::set_immediate_viewport_renderer(|ctx, viewport| {});
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
            string_name!("timeout"),
            Callable::from_object_method(
                &self.to_gd(),
                string_name!(Self, __internal_finish_frame_inner),
            ),
        );
    }

    fn finish_frame(&self) {
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

impl Context {
    fn repaint(&self, info: egui::RequestRepaintInfo) {}

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

/* ------------------------------------------ Viewport ------------------------------------------ */

struct ViewportContext {}
