use std::{
    collections::{hash_map, HashSet, VecDeque},
    mem::take,
    sync::{
        atomic::{AtomicBool, AtomicU64, Ordering::Relaxed},
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

use crate::{
    default,
    painter::{self, ViewportInput},
};

/* ---------------------------------------------------------------------------------------------- */
/*                                             BRIDGE                                             */
/* ---------------------------------------------------------------------------------------------- */

/// Primary Egui Interface.
#[derive(GodotClass)]
#[class(base=CanvasLayer, init, rename=GodotEguiBridge)]
pub struct EguiBridge {
    base: Base<engine::CanvasLayer>,
    share: Arc<SharedContext>,

    /// Number of bits allowed for texture size.
    ///
    /// The texture will be 2^max_texture_size
    #[export]
    #[var(get, set)]
    #[init(default = 13)]
    pub max_texture_bits: u8,

    /// Actual Godot Nodes for realization of viewports.
    ///
    /// # NOTE
    ///
    /// Lock order MUST be `share.viewports` -> `painters.`
    painters: Mutex<ViewportIdMap<PainterContext>>,
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

    /// Sets at the very first frame.
    builder: egui::ViewportBuilder,
}

struct ViewportContext {
    /// Repainted when time point reaches here.
    repaint_at: Option<Instant>,

    /// Repaint method, that will be called when ready.
    repaint_with: Option<Arc<DeferredViewportUiCallback>>,

    /// Any input captures from viewport.
    rx_update: mpsc::Receiver<painter::ViewportInput>,

    /// Viewport initialization
    builder: egui::ViewportBuilder,

    /// Viewport commands pending apply. When should be recreated, the second parameter
    /// set to [`Some`].
    updates: (
        Vec<egui::ViewportCommand>,
        Option<mpsc::Sender<painter::ViewportInput>>,
    ),

    /// Paint commands that is being applied,
    paint_this_frame: Option<oneshot::Receiver<Vec<egui::ClippedPrimitive>>>,

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
type FnShowWidget = dyn 'static + Send + Fn(&egui::Ui);

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

    /// Spawn a widget on main draw loop.
    ///
    /// Passed `config` will be used for creating container window for widget. To control
    /// window visibility / close behavior, use bool reference parameter to call
    /// [`egui::Window::open`].
    ///
    /// If you set delivered boolean parameter of `configure` as false, then the window
    /// will be closed.
    pub fn spawn_main_widget(
        &self,
        configure: impl 'static + Send + for<'a> Fn(&'a mut bool) -> egui::Window,
        show: impl 'static + Send + FnMut(&egui::Ui),
    ) {
        let show = Mutex::new(show);
        self.share.spawned_widgets.lock().pipe(|mut entries| {
            entries.push(SpawnedWidgetInfo {
                build_window: Box::new(configure),
                show: Box::new(move |ui| show.lock()(ui)),
            })
        });
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
        show: impl FnMut(&egui::Context) -> bool + 'static + Send + Sync,
    ) {
        // Spawn a viewport which is retained as long as show returns true.
        self.share.spawned_viewports.lock().pipe(|mut table| {
            let dispose = Arc::new(AtomicBool::new(false));
            let show_fn = Mutex::new(show);

            table.insert(
                id,
                SpawnedViewportContext {
                    dispose: dispose.clone(),
                    repaint: Arc::new(move |ctx| {
                        dispose.store(show_fn.lock()(ctx), Relaxed);
                    }),
                    builder,
                },
            )
        });

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

            assert!(this.viewport_start_frame(
                viewport.ids.this,
                Some(viewport.builder),
                default()
            ));
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
                let mut inp = share.raw_input_template.lock();
                inp.viewports = vp;
                inp.time = Some(engine::Time::singleton().get_ticks_usec() as f64 / 1e6);

                // XXX: 256~ 65536 texture size limitation => is this practical?
                inp.max_texture_side = Some(1 << (self.max_texture_bits as usize).clamp(8, 16));
            });

        // Start root frame as normal.
        assert!(self.viewport_start_frame(egui::ViewportId::ROOT, None, default()));
    }

    fn finish_frame(&mut self) {
        let share = self.share.clone();

        // Show main frame widgets:
        //
        // We temporarily take out of all widgets from array, to ensure no deadlock
        // would occur!
        take(&mut *share.spawned_widgets.lock())
            .tap_mut(|widgets| {
                widgets.retain_mut(|w| {
                    let mut keep_open = true;
                    let wnd = (w.build_window)(&mut keep_open);
                    let resp = wnd.show(&share.egui, |ui| (w.show)(ui));

                    let Some(resp) = resp else {
                        // Widget is closed.
                        return false;
                    };

                    if resp.response.gained_focus() || resp.response.clicked() {
                        // TODO: Check if changing drawing order meaningful.
                        // - Seems handled by egui?
                    }

                    true
                })
            })
            .pipe(|mut widgets| {
                // Not put it back to original array.
                let mut lock = share.spawned_widgets.lock();

                // Prevent allocation when possible.
                widgets.extend(lock.drain(..));
                *lock = widgets;
            });

        // Deal with spawned viewports; in same manner.
        take(&mut *share.spawned_viewports.lock())
            .tap_mut(|viewports| {
                viewports.retain(|id, value| {
                    if value.dispose.load(Relaxed) {
                        false
                    } else {
                        let ui_cb = value.repaint.clone();
                        share.egui.show_viewport_deferred(
                            *id,
                            value.builder.clone(),
                            move |ctx, _| {
                                ui_cb(ctx);
                            },
                        );

                        true
                    }
                });
            })
            .pipe(|mut viewports| {
                let mut lock = share.spawned_viewports.lock();

                // Overwrite previous viewports with newly spawned ones, if exist.
                viewports.extend(lock.drain());
                *lock = viewports;
            });

        // End main frame loop.
        self.viewport_end_frame(egui::ViewportId::ROOT);

        // TODO: Handle viewport changes from output, visit each viewports
        let mut remaining_viewports = share
            .viewports
            .lock()
            .keys()
            .copied()
            .collect::<HashSet<_>>();

        let mut viewports = VecDeque::new();

        loop {
            viewports.extend(share.full_output.lock().viewport_output.drain());

            let Some((vp_id, vp_out)) = viewports.pop_front() else {
                break;
            };

            if !remaining_viewports.remove(&vp_id) {
                // This viewport seems created multiple times ...
                godot_warn!("Viewport {vp_id:?} declared multiple times from different root");
                continue;
            }

            // TODO: Deal with commands ...
        }

        // TODO: Cleanup full_output for next frame.

        // TODO: Handle new textures from output.

        // TODO: Show all viewports.

        // TODO: Paint all viewports

        // TODO: Handle disposed textures from output.

        // Then it's ready to start next frame.
        self.share.join_frame_fence();
    }

    fn viewport_start_frame(
        &self,
        id: ViewportId,
        build: Option<ViewportBuilder>,
        commands: Vec<egui::ViewportCommand>,
    ) -> bool {
        // TODO: Check schedule; if it's okay to start rendering.
        let mut should_render = id == ViewportId::ROOT;

        // NOTE: Seems recursive call to begin_frame is handled by stack internally.
        let mut raw_input = self.share.raw_input_template.lock().clone();

        // Spawn context if viewport id not exist
        let mut viewport_lock = self.share.viewports.lock();
        let viewport = match viewport_lock.entry(id) {
            hash_map::Entry::Occupied(mut entry) => {
                if let Some(build) = build {
                    let entry = entry.get_mut();
                    let (patch, recreate) = entry.builder.patch(build);

                    if recreate || !patch.is_empty() {
                        should_render |= true;
                    }

                    if recreate {
                        // Que recreation
                        let (tx_update, rx_update) = mpsc::channel();

                        entry.updates.0 = patch;
                        entry.updates.1 = Some(tx_update);
                        entry.rx_update = rx_update;
                    } else {
                        entry.updates.0.extend(patch);
                    }
                }

                entry.into_mut()
            }
            hash_map::Entry::Vacant(entry) => {
                should_render |= true;

                let (tx_update, rx_update) = mpsc::channel();
                let mut init = ViewportBuilder::default();

                let (updates, _) = build.map(|x| init.patch(x)).unwrap_or_default();

                entry.insert(ViewportContext {
                    repaint_at: Some(Instant::now()),
                    repaint_with: None,
                    rx_update,
                    builder: init,
                    updates: (updates, Some(tx_update)),
                    paint_this_frame: None,
                    info: default(),
                })
            }
        };

        // Spawn or update viewport
        viewport.updates.0.extend(commands);

        let mut painter = self.painters.lock().entry(id);

        // Check if we have to create new

        // It's highly likely be non-deferred renderer. Force rendering always!
        should_render |= viewport.repaint_with.is_none();

        raw_input.screen_rect = viewport.info.inner_rect;
        raw_input.focused = viewport.info.focused.unwrap_or_default();
        raw_input.viewport_id = id;

        // TODO: Collect inputs from given viewport

        // Check rendering schedule for deferred viewport
        if !should_render && viewport.repaint_at.is_some_and(|x| Instant::now() < x) {
            // Only if it's deferred viewport(repaint is set) and repaint schedule not reached;
            return false;
        }

        if should_render {
            self.share.egui.begin_frame(raw_input);
            true
        } else {
            false
        }
    }

    fn free_painter(painter: PainterContext) {
        // todo!()
    }

    fn viewport_end_frame(&self, id: ViewportId) {
        // Retrieve viewport-wise output.
        let mut output = self.share.egui.end_frame();

        let paints = take(&mut output.shapes);
        let ppi = output.pixels_per_point;

        // Que async tesselation jobs to Rayon.
        let egui = self.share.egui.clone();
        let (tx, rx) = oneshot::channel();

        self.share
            .viewports
            .lock()
            .get_mut(&id)
            .unwrap()
            .paint_this_frame = Some(rx);

        // Offload tesselation from game thread.
        rayon::spawn_fifo(move || {
            // It's just okay to fail.
            tx.send(egui.tessellate(paints, ppi)).ok();
        });

        // Accumulate outputs to primary output.
        self.share.full_output.lock().append(output);
    }

    fn paint_viewport(&mut self, id: ViewportId) {
        // TODO: Spawn viewport node if not found from painters.

        // TODO: From tesselation result, draw painter.
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

    fn join_frame_fence(&self) {
        let (ref mut current, ref mut requested) = *self.fence_frame.lock();
        *requested = *current;
    }
}
