use std::{
    cell::{Cell, RefCell},
    collections::{hash_map, HashSet, VecDeque},
    mem::take,
    sync::{
        atomic::{AtomicBool, AtomicU8, Ordering::Relaxed},
        mpsc, Arc,
    },
    thread::ThreadId,
    time::{Duration, Instant},
};

use educe::Educe;
use egui::{
    mutex::Mutex, CursorIcon, DeferredViewportUiCallback, ViewportBuilder, ViewportClass,
    ViewportId, ViewportIdMap,
};
use godot::{
    engine::{
        self,
        control::{LayoutPreset, MouseFilter},
        window, CanvasLayer, Control, DisplayServer, ICanvasLayer, WeakRef,
    },
    prelude::*,
};
use tap::prelude::{Pipe, Tap};
use with_drop::with_drop;

use crate::{
    default,
    helpers::{downgrade_gd, try_upgrade_gd, ToCounterpart},
    surface,
};

/* ---------------------------------------------------------------------------------------------- */
/*                                             BRIDGE                                             */
/* ---------------------------------------------------------------------------------------------- */

/// Primary Egui Interface.
#[derive(GodotClass)]
#[class(base=CanvasLayer, tool, init, rename=GodotEguiBridge)]
pub struct EguiBridge {
    base: Base<CanvasLayer>,

    /// Requires [`Arc`] for egui callback requirements.
    share: Arc<SharedContext>,

    /// Number of bits allowed for texture size.
    ///
    /// The texture will be 2^max_texture_size
    #[export]
    #[var(get, set)]
    #[init(default = 13)]
    pub max_texture_bits: u8,

    /// Texture storage
    textures: surface::TextureLibrary,

    /// Pending intra-frame access methods

    /// Actual Godot Nodes for realization of viewports.
    ///
    /// # NOTE
    ///
    /// Lock order MUST be `share.viewports` -> `painters.`
    surfaces: RefCell<ViewportIdMap<SurfaceContext>>,

    /// Setup scripts that was deferred until next frame end.
    setup_scripts: RefCell<Vec<Box<FnDeferredContextAccess>>>,

    /// Determines the cursor shape of this frame.
    cursor_shape: RefCell<Option<egui::CursorIcon>>,

    /// Handle to tesselation worker thread
    thr_bg_worker: Cell<Option<std::thread::JoinHandle<()>>>,

    /// Handle to tesselation worker tx channel
    tx_bg_task: RefCell<Option<mpsc::Sender<BackgroundWorkCommand>>>,

    /// A object that root region is being synced.
    root_region_sync: Cell<Option<Gd<WeakRef>>>,

    /// non-send + non-sync
    _non_send_sync: std::marker::PhantomData<*const ()>,
}

enum BackgroundWorkCommand {
    RequestRepaint(ViewportId),
    Tessellate(
        Vec<egui::epaint::ClippedShape>,
        f32,
        oneshot::Sender<Vec<egui::ClippedPrimitive>>,
    ),
    Close, // Close the worker thread channel.
}

#[derive(Clone)]
struct SurfaceContext {
    /// Actual painter window.
    painter: Gd<surface::EguiViewportBridge>,

    /// Container window if exist.
    window: Option<Gd<engine::Window>>,
}

#[derive(Educe)]
#[educe(Default)]
struct SharedContext {
    egui: egui::Context,

    /// Detects whether to start new frame.
    frame_started: AtomicBool,

    /// Template input for each viewport rendering.
    raw_input_template: Mutex<egui::RawInput>,

    /// Accumulated output for entire single frame.
    full_output: Mutex<egui::FullOutput>,

    /// List of viewports that is tracked by this context.
    spawned_viewports: Mutex<ViewportIdMap<SpawnedViewportContext>>,

    /// List of viewports that
    viewports: Mutex<ViewportIdMap<ViewportContext>>,

    /// The thread ID that instance was initiated.
    #[educe(Default = std::thread::current().id())]
    main_thread_id: ThreadId,
}

struct SpawnedViewportContext {
    /// Captures `dispose`, then set it to false when viewport closed.
    repaint: Arc<DeferredViewportUiCallback>,

    /// Should spawned viewport be closed?
    dispose: Arc<Mutex<WidgetRetain>>,

    /// Sets at the very first frame.
    builder: egui::ViewportBuilder,
}

struct ViewportContext {
    /// Repainted when time point reaches here.
    repaint_at: Option<Instant>,

    /// Any input captures from viewport.
    rx_update: mpsc::Receiver<egui::Event>,

    /// Viewport initialization
    builder: egui::ViewportBuilder,

    /// Close request status
    close_request: Arc<ViewportClose>,

    /// Viewport commands pending apply. When should be recreated, the second parameter
    /// set to [`Some`].
    updates: Vec<egui::ViewportCommand>,

    /// Paint commands that is being applied,
    paint_this_frame: Option<oneshot::Receiver<Vec<egui::ClippedPrimitive>>>,

    /// Logical zoom rate requested by user. The painter will accept and rescale event
    /// position and paintings to fit the zoom rate.
    target_ui_scale: f32,

    /// Cached viewport information, that we're currently updating on.
    info: egui::ViewportInfo,
}

/// Closing steps
///
/// 1. Requested: Godot Window sends close signal => `ViewportContext::close_request`
///    (=flag) is set to `VIEWPORT_CLOSE_REQUESTED`
/// 2. Next start of frame: `VIEWPORT_CLOSE_REQUESTED` is detected, then it sets to
///    `PENDING`, delivering `egui::ViewportEvent::Close` to make user detect if it's
///    closing
/// 3. If User don't want the viewport to be closed, user can send
///    `ViewportCommand::CancelClose` to cancel the close request.
/// 4. If not canceled, then the same frame, `PENDING` transitions to `CLOSE`, which will
///    be disposed on next frame's `finish_frame` call.
type ViewportClose = AtomicU8;

const VIEWPORT_CLOSE_NONE: u8 = 0;
const VIEWPORT_CLOSE_REQUESTED: u8 = 1;
const VIEWPORT_CLOSE_PENDING: u8 = 2;
const VIEWPORT_CLOSE_CLOSE: u8 = 3;

/// Callback for deferred context access, for non-rendering purposes.
type FnDeferredContextAccess = dyn FnOnce(&egui::Context) + 'static;

/* --------------------------------- Widget Lifetime Control -------------------------------- */

/// Every spawned widgets are retained as long as the callback returns true.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum WidgetRetain {
    Retain,
    Dispose,

    /// For widgets, it is treated as `Retain` permanently. For viewports, it'll be
    /// disposed at the end of frame.
    #[default]
    Unspecified,
}

impl WidgetRetain {
    pub fn and(self, other: Self) -> Self {
        match (self, other) {
            (Self::Dispose, _) | (_, Self::Dispose) => Self::Dispose,
            (Self::Retain, _) | (_, Self::Retain) => Self::Retain,
            _ => Self::Unspecified,
        }
    }

    pub fn disposed(&self) -> bool {
        matches!(self, Self::Dispose)
    }
}

impl From<bool> for WidgetRetain {
    fn from(x: bool) -> Self {
        if x {
            Self::Retain
        } else {
            Self::Dispose
        }
    }
}

impl From<()> for WidgetRetain {
    fn from(_: ()) -> Self {
        Self::Unspecified
    }
}

/* ------------------------------------------ Godot Api ----------------------------------------- */

#[godot_api]
impl ICanvasLayer for EguiBridge {
    fn process(&mut self, _dt: f64) {
        if self.share.is_in_frame() {
            self.finish_frame();
        }
    }

    fn enter_tree(&mut self) {
        self.try_initiate();
    }

    fn exit_tree(&mut self) {
        self.try_dispose();
    }
}

#[godot_api]
impl EguiBridge {
    #[func]
    fn __internal_try_start_frame_inner(&self) {
        self.try_start_frame();
    }
}

/* -------------------------------------------- APIs -------------------------------------------- */

/// APIs for spawning viewports.
///
/// Key for every APIs are that any access to [`egui::Context`] triggers
impl EguiBridge {
    /// Access to egui context at intra-frame. This will be called immediately if we're
    /// already out of frame boundary(e.g. start..end), otherwise, queue it to be called
    /// later.
    pub fn setup_context(&self, setter: impl FnOnce(&egui::Context) + 'static + Send) {
        if self.share.is_in_frame() {
            self.setup_scripts.borrow_mut().push(Box::new(setter));
        } else {
            setter(&self.share.egui);
        }
    }

    /// Synchronize root viewport's region with given control. If [`None`] is given, it
    /// unregisters synchronization.
    pub fn sync_root_region(&self, target: Option<Gd<Control>>) {
        if let Some(target) = target {
            self.root_region_sync.set(Some(downgrade_gd(target)));
        } else {
            self.reset_root_region_sync();
        }
    }

    /// Start a new frame (if required), and return context which you can draw with.
    ///
    /// This is very default way of using EGUI, and anything you draw upon this will be
    /// shown below the spawned root canvas; [`EguiBridge`]
    ///
    /// Use this when you want to draw widget every frame within `process()` function.
    ///
    /// # Caveats
    ///
    /// - Cloning `egui::Context` and access it directly out of provided lifecycle is not
    ///   recommneded. Please guarantee that you only access this context within main
    ///   thread, right after calling `current_frame`.
    ///
    /// # Panics
    ///
    /// - Called from non main gameplay thread.
    pub fn current_frame(&self) -> &egui::Context {
        self.try_start_frame();

        &self.share.egui
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
    pub fn viewport_immediate<R>(
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
    pub fn viewport_spawn<L>(
        &self,
        id: ViewportId,
        builder: ViewportBuilder,
        show: impl FnMut(&egui::Context) -> L + 'static,
    ) where
        L: Into<WidgetRetain>,
    {
        // Spawn a viewport which is retained as long as show returns true.
        self.share.spawned_viewports.lock().pipe(|mut table| {
            let dispose = Arc::new(Mutex::new(WidgetRetain::default()));
            let show_fn = FnWrapSendSync(show);
            let show_fn = Mutex::new(show_fn);

            struct FnWrapSendSync<F>(pub F);

            // SAFETY: EguiBridge can't escape main thread
            // + All viewport methods are invoked from main thread, and never touches
            //   other thread.
            unsafe impl<F> Send for FnWrapSendSync<F> {}

            table.insert(
                id,
                SpawnedViewportContext {
                    dispose: dispose.clone(),
                    repaint: Arc::new(move |ctx| {
                        *dispose.lock() = show_fn.lock().0(ctx).into();
                    }),
                    builder,
                },
            )
        });

        // Ensure the ui frame gets
        self.queue_try_start_frame();
    }

    /// Spawn new viewport as child of existing node. If specified parent node is behind
    /// other node, the input may work naturally as the egui surface always intercepts any
    /// GUI input. It is advised to use this method for any node that lays over any other
    /// GUI nodes, which makes all egui rendering appear always top of the other GUI
    /// nodes.
    pub fn viewport_spawn_as_child(
        &self,
        _id: ViewportId,
        _parent: Gd<Control>,
        _builder: ViewportBuilder,
        _show: impl FnOnce(&egui::Context) -> WidgetRetain + 'static,
    ) {
    }

    /// Attach given node to given viewport's window.
    ///
    /// TODO: Implement this!
    pub fn attach_node_to_viewport(&self, _id: ViewportId, node: Gd<Node>) -> Result<(), Gd<Node>> {
        Err(node)
    }
}

/* ------------------------------------------ Privates ------------------------------------------ */

/// Private API implementations
impl EguiBridge {
    fn try_initiate(&self) {
        if self.tx_bg_task.borrow().is_some() {
            // It's already initiated.
            return;
        }

        // Spawn background worker thread
        {
            let (tx_b, rx_b) = std::sync::mpsc::channel::<BackgroundWorkCommand>();
            let ctx = self.share.egui.clone();
            let th = std::thread::spawn(move || loop {
                // Channel is always valid until `Close` command is received. Otherwise,
                // it's a bug where `_exit_tree` is not invoked from engine.
                let msg = rx_b.recv().expect("unexpected channel close");

                match msg {
                    BackgroundWorkCommand::RequestRepaint(viewport_id) => {
                        ctx.request_repaint_of(viewport_id);
                    }
                    BackgroundWorkCommand::Tessellate(shapes, ppi, tx_response) => {
                        let primitives = ctx.tessellate(shapes, ppi);
                        tx_response.send(primitives).ok();
                    }
                    BackgroundWorkCommand::Close => break,
                }
            });

            assert!(self.tx_bg_task.replace(Some(tx_b)).is_none());
            assert!(self.thr_bg_worker.replace(Some(th)).is_none());
        };

        // Setup egui context & repaint callback.
        (&self.share.egui).pipe(|ctx| {
            let w_share = Arc::downgrade(&self.share);

            ctx.set_embed_viewports(false);
            ctx.set_request_repaint_callback({
                // Prevent cyclic reference; `share` is already holding the reference!
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

    fn try_dispose(&mut self) {
        // Join background worker thread. To do this, channel should be closed first.
        let Some(bg) = self.tx_bg_task.take() else {
            // Service is just not initialized.
            return;
        };

        bg.send(BackgroundWorkCommand::Close)
            .expect("channel closed prematurely");

        let bg_thr = self.thr_bg_worker.take().expect("thread not spawned");
        bg_thr.join().expect("thread panicked");

        // Cleanup all godot resources
        self.textures.clear();

        self.share.viewports.lock().clear();
        self.share.spawned_viewports.lock().clear();

        for (_, surface) in self.surfaces.borrow_mut().drain() {
            Self::free_surface(Some(surface));
        }
    }

    fn try_start_frame(&self) {
        assert!(std::thread::current().id() == self.share.main_thread_id);

        // Only perform frame start when necessary.
        if !self.share.try_advance_frame() {
            return;
        }

        // Just lazily initiate the system.
        self.try_initiate();

        // Register immediate renderer for this frame.
        let w_self = utilities::weakref(self.to_gd().to_variant());

        // TODO: Make this not to capture anything; instead let it retrieve required data
        // from `ctx`
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

            this.viewport_validate(
                viewport.ids.this,
                Some((viewport.ids.parent, viewport.builder)),
            );
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
                let mut inp = share.raw_input_template.lock();
                inp.viewports = vp;
                inp.time = Some(engine::Time::singleton().get_ticks_usec() as f64 / 1e6);

                // XXX: 256~ 65536 texture size limitation => is this practical?
                inp.max_texture_side = Some(1 << (self.max_texture_bits as usize).clamp(8, 16));
                inp.modifiers = {
                    use engine::global::Key as GdKey;

                    let gd_input = engine::Input::singleton();
                    let is_pressed = |k: GdKey| gd_input.is_key_pressed(k);

                    egui::Modifiers {
                        alt: is_pressed(GdKey::ALT),
                        ctrl: is_pressed(GdKey::CTRL),
                        shift: is_pressed(GdKey::SHIFT),
                        command: is_pressed(GdKey::CTRL),
                        mac_cmd: is_pressed(GdKey::META),
                    }
                };
            });

        // Before starting a frame, check if we can spawn separate windows for viewport.
        self.share.egui.set_embed_viewports(
            self.base()
                .get_viewport()
                .unwrap()
                .is_embedding_subwindows(),
        );

        // Start root frame as normal.
        self.viewport_validate(egui::ViewportId::ROOT, None);

        // After root region is initialized, try sync it with root region.
        'sync: {
            let Some(w_target) = self.root_region_sync.take() else {
                break 'sync;
            };

            let Some(target) = try_upgrade_gd::<Control>(w_target.clone()) else {
                self.reset_root_region_sync();
                break 'sync;
            };

            // Target is still valid; return it back to the list.
            self.root_region_sync.set(Some(w_target));

            // Check if size mismatches
            let mut surfaces = self.surfaces.borrow_mut();
            let root = surfaces.get_mut(&ViewportId::ROOT).unwrap();

            let target_rect = target.get_global_rect();
            let root_rect = root.painter.get_global_rect();

            let err_pos = target_rect.position - root_rect.position;
            let err_size = target_rect.size - root_rect.size;

            // We use error approximation here as the sync size is result of calculation
            // => which may have floating point errors.
            if err_pos.length_squared() < 1e-4 && err_size.length_squared() < 1e-4 {
                // No need to sync
                break 'sync;
            }

            // Sync root region
            root.painter.set_global_position(target_rect.position);
            root.painter.set_size(target_rect.size);
        }

        self.viewport_start_frame(egui::ViewportId::ROOT);

        // NOTE: Implementation is too long, thus splitting it into another function.

        // Handle spawned widgets most first; as it should not be affected by any other controls!
        // self._start_frame_handle_widgets();
    }

    fn reset_root_region_sync(&self) {
        self.surfaces
            .borrow_mut()
            .get_mut(&egui::ViewportId::ROOT)
            .unwrap()
            .pipe(|x| {
                x.painter
                    .set_anchors_and_offsets_preset(LayoutPreset::FULL_RECT)
            });

        // just to ensure.
        self.root_region_sync.set(None);
    }

    fn finish_frame(&mut self) {
        let share = self.share.clone();

        /* ------------------------- Spawned Widget / Viewport Handling ------------------------- */

        let viewports = take(&mut *share.spawned_viewports.lock()).tap_mut(|viewports| {
            // Check if any of the spawned viewports should be disposed.
            viewports.retain(|id, value| {
                if *value.dispose.lock() == WidgetRetain::Dispose {
                    false
                } else {
                    let ui_cb = value.repaint.clone();
                    share
                        .egui
                        .show_viewport_deferred(*id, value.builder.clone(), move |ctx, _| {
                            ui_cb(ctx);
                        });

                    true
                }
            });
        });

        viewports.pipe(|mut viewports| {
            // Check-in viewports list.
            let mut lock = share.spawned_viewports.lock();

            // Overwrite previous viewports with newly spawned ones, if exist.
            viewports.extend(lock.drain());
            *lock = viewports;
        });

        /* ------------------------------ Viewport Deltas Handling ------------------------------ */

        // End main frame loop.
        self.viewport_end_frame(egui::ViewportId::ROOT);

        // Handle viewport changes from output, visit each viewports
        let mut remaining_viewports = share
            .viewports
            .lock()
            .keys()
            .copied()
            .collect::<HashSet<_>>();

        let mut viewports = VecDeque::new();
        let now = Instant::now();

        loop {
            // Not any lock should be held here.

            viewports.extend(share.full_output.lock().viewport_output.drain());

            let Some((vp_id, vp_out)) = viewports.pop_front() else {
                break;
            };

            let scheduled = if let Some(viewport) = share.viewports.lock().get_mut(&vp_id) {
                if viewport.close_request.load(Relaxed) == VIEWPORT_CLOSE_CLOSE {
                    // If this is `PENDING`, it means the user side renderer has already
                    // seen viewport close request, however, didn't deal with it, which
                    // means accepted disposal of viewport close.

                    // Simply by not invoking subsequent rendering logic, (more precisely,
                    // not removing viewport ID from `remaining_viewports`), we can safely
                    // dispose this viewport.
                    continue;
                }

                // Commands are only meaningful when viewport already present.
                viewport.updates.extend(vp_out.commands);

                if viewport.repaint_at.is_some_and(|x| x < now) {
                    viewport.repaint_at = None; // Clear repaint timer until next request
                    true
                } else {
                    // If viewport is being closed now, force repainting it.
                    viewport.close_request.load(Relaxed) == VIEWPORT_CLOSE_REQUESTED
                }
            } else {
                false
            };

            // Don't need to check if remove succeeded; as it can be a viewport created
            // inside rendering loop; which is perfectly valid egui API call.
            let _ = remaining_viewports.remove(&vp_id);

            // Validate viewport.
            self.viewport_validate(vp_id, Some((vp_out.parent, vp_out.builder)));

            if let Some(ui_cb) = vp_out.viewport_ui_cb.filter(|_| scheduled) {
                // Check if we should repaint this deferred viewport. For root and
                // immediate viewports, these methods are already invoked!

                self.viewport_start_frame(vp_id);
                // Populate renderings
                ui_cb(&self.share.egui);
                self.viewport_end_frame(vp_id);
            }
        }

        // Deal with removed viewports
        for id in remaining_viewports {
            match share.spawned_viewports.lock().entry(id) {
                hash_map::Entry::Occupied(entry) => {
                    if *entry.get().dispose.lock() == WidgetRetain::Retain {
                        // The widget didn't agree to close, so we put it back to the list.
                        // Other than `Retain` treated as `Dispose`.
                        continue;
                    }

                    // Spawned viewport also agreed to close.
                    entry.remove();
                }
                hash_map::Entry::Vacant(_) => (),
            }

            // Painter should be freed first, then viewport.
            Self::free_surface(self.surfaces.borrow_mut().remove(&id));

            // Remove viewport from context. Assertion here since we've retrieved
            // remaining_viewports from viewport list itself, any 'subtractive'
            // modification on viewports list is internal logic error!
            assert!(share.viewports.lock().remove(&id).is_some());
        }

        /* -------------------------------- Frame Output Handling ------------------------------- */

        // Cleanup full_output for next frame.
        let egui::FullOutput {
            platform_output: _,
            textures_delta:
                egui::TexturesDelta {
                    set: textures_created,
                    free: textures_freed,
                },
            shapes,
            pixels_per_point: _,
            viewport_output: _,
        } = take(&mut *self.share.full_output.lock());

        debug_assert!(shapes.is_empty(), "logic error - shape is viewport-wise");

        // Handle cursor shape
        if let Some(cursor) = self.cursor_shape.take() {
            type CS = engine::display_server::CursorShape;
            let mut ds = DisplayServer::singleton();

            ds.cursor_set_shape(match cursor {
                egui::CursorIcon::Default => CS::ARROW,
                // egui::CursorIcon::None =>
                // egui::CursorIcon::ContextMenu => CursorShape::meu,
                egui::CursorIcon::Help => CS::HELP,
                egui::CursorIcon::PointingHand => CS::POINTING_HAND,
                // egui::CursorIcon::Progress =>
                egui::CursorIcon::Wait => CS::WAIT,
                // egui::CursorIcon::Cell =>
                egui::CursorIcon::Crosshair => CS::CROSS,
                egui::CursorIcon::Text => CS::IBEAM,
                egui::CursorIcon::VerticalText => CS::IBEAM,
                // egui::CursorIcon::Alias => CS::,
                // egui::CursorIcon::Copy =>
                // egui::CursorIcon::Move =>
                // egui::CursorIcon::NoDrop =>
                egui::CursorIcon::NotAllowed => CS::FORBIDDEN,
                // egui::CursorIcon::Grab => ,
                // egui::CursorIcon::Grabbing =>
                egui::CursorIcon::AllScroll => CS::MOVE,
                egui::CursorIcon::ResizeHorizontal => CS::HSIZE,
                egui::CursorIcon::ResizeNeSw => CS::BDIAGSIZE,
                egui::CursorIcon::ResizeNwSe => CS::FDIAGSIZE,
                egui::CursorIcon::ResizeVertical => CS::VSIZE,
                egui::CursorIcon::ResizeEast => CS::HSIZE,
                egui::CursorIcon::ResizeSouthEast => CS::FDIAGSIZE,
                egui::CursorIcon::ResizeSouth => CS::VSIZE,
                egui::CursorIcon::ResizeSouthWest => CS::BDIAGSIZE,
                egui::CursorIcon::ResizeWest => CS::HSIZE,
                egui::CursorIcon::ResizeNorthWest => CS::FDIAGSIZE,
                egui::CursorIcon::ResizeNorth => CS::VSIZE,
                egui::CursorIcon::ResizeNorthEast => CS::BDIAGSIZE,
                egui::CursorIcon::ResizeColumn => CS::HSIZE,
                egui::CursorIcon::ResizeRow => CS::VSIZE,
                // egui::CursorIcon::ZoomIn =>
                // egui::CursorIcon::ZoomOut =>
                _cursor => CS::ARROW,
            });
        }

        /* -------------------------------------- Painting -------------------------------------- */

        // Handle new textures from output.
        for (id, delta) in textures_created {
            self.textures.update_texture(id, delta);
        }

        // Paint all viewports
        for (id, mut paint) in self.surfaces.borrow_mut().clone() {
            let Some((rx_primitives, ui_scale)) = self
                .share
                .viewports
                .lock()
                .get_mut(&id)
                .unwrap()
                .pipe(|vp| vp.paint_this_frame.take().map(|x| (x, vp.target_ui_scale)))
            else {
                // This viewport is not re-rendered this frame.
                continue;
            };

            // Wait for tesselation result synchronously.
            let Ok(primitives) = rx_primitives.recv_timeout(Duration::from_secs(5)) else {
                godot_warn!(
                    "Tesselation result for viewport {id:?} is not received within 5 seconds"
                );
                continue;
            };

            paint
                .painter
                .bind_mut()
                .draw(&self.textures, primitives, ui_scale);
        }

        // Handle disposed textures from output.
        for id in textures_freed {
            self.textures.free_texture(id);
        }

        /* ---------------------------------------- Done. --------------------------------------- */

        // Finish this frame.
        self.share.finish_frame();
    }

    fn free_surface(x: Option<SurfaceContext>) {
        if let Some(mut x) = x {
            x.painter.queue_free();

            if let Some(mut x) = x.window {
                x.queue_free();
            }
        }
    }

    fn viewport_validate(
        &self,
        id: ViewportId,
        build_with_parent: Option<(ViewportId, ViewportBuilder)>,
    ) {
        // Checkout painter
        let mut surface = with_drop(self.surfaces.borrow_mut().remove(&id), Self::free_surface);

        // Spawn context if viewport id not exist
        let mut should_rebuild = false;
        let mut viewport_lock = self.share.viewports.lock();
        let viewport = match viewport_lock.entry(id) {
            hash_map::Entry::Occupied(mut entry) => {
                if let Some((parent, build)) = build_with_parent {
                    let entry = entry.get_mut();
                    let (patch, recreate) = entry.builder.patch(build);

                    // We don't need to trigger recreation from this flag ... Everything
                    // is configurable through commands.
                    let _ = recreate;

                    if entry.info.parent.is_some_and(|p| p != parent) {
                        // Parent is changed, so we need to recreate this viewport.
                        should_rebuild = true;

                        // In this case, previous updates will be discarded.
                        entry.updates.splice(.., patch);
                    } else {
                        entry.updates.extend(patch);
                    }
                }

                entry.into_mut()
            }
            hash_map::Entry::Vacant(entry) => {
                should_rebuild = true;

                #[cfg(any())]
                godot_print!(
                    "spawning new EGUI viewport: {id:?} / {}",
                    build_with_parent
                        .as_ref()
                        .map(|x| x.1.title.as_deref().unwrap_or("<unnamed>"))
                        .unwrap_or("ROOT")
                );

                // Just throw away this ... it'll be replaced by new one.
                let (_tx_update, rx_update) = mpsc::channel();
                let mut init = ViewportBuilder::default();

                // Derive some defaults from parent window
                let gd_wnd_parent = build_with_parent
                    .as_ref()
                    .map(|x| x.0)
                    .and_then(|id| {
                        self.surfaces
                            .borrow_mut()
                            .get(&id)
                            .and_then(|x| x.window.clone())
                    })
                    .unwrap_or_else(|| self.base().get_window().expect("not added in tree!"));

                let (updates, _) = build_with_parent
                    .map(|x| {
                        init.patch(x.1.tap_mut(|init| {
                            if init.position.is_none() {
                                let pos = gd_wnd_parent.get_position().to_alternative();
                                init.position = Some(pos + egui::vec2(25., 25.));
                            }

                            if init.inner_size.is_none() {
                                init.inner_size = Some(egui::vec2(272., 480.));
                            }
                        }))
                    })
                    .unwrap_or_default();

                entry.insert(ViewportContext {
                    repaint_at: Some(Instant::now()),
                    rx_update,
                    close_request: Default::default(),
                    builder: init,
                    target_ui_scale: 1.,
                    updates,
                    paint_this_frame: None,
                    info: default(),
                })
            }
        };

        if surface.is_none() || should_rebuild {
            drop(surface.take());

            // Create channel between new viewport and painter.
            let (tx_viewport, rx_viewport) = mpsc::channel();
            viewport.rx_update = rx_viewport;

            // Rebuild UI.
            let mut gd_painter = surface::EguiViewportBridge::new_alloc();

            let ctx = self.share.egui.clone();
            gd_painter.bind_mut().initiate(
                ctx.clone(),
                Box::new(move |ev| {
                    // NOTE: cloning egui context into this closure doesn't make cyclic reference
                    // - Both are field of `Self`, which does not refer to each other.

                    tx_viewport.send(ev).ok(); // Failing this is just okay.
                    ctx.request_repaint_of(id);
                }),
            );

            let tx = self.tx_bg_task.borrow().clone().unwrap();
            gd_painter.connect(
                "resized".into(),
                Callable::from_fn("Resize", move |_| {
                    // Send repaint request to background worker. Here we don't directly
                    // call `Context::request_repaint` method on context object to prevent
                    // deadlock, as we're not sure when this bound method is called. (it
                    // actually deadlocks on widget initialization)
                    tx.send(BackgroundWorkCommand::RequestRepaint(id)).ok();
                    Ok(Variant::nil())
                }),
            );

            let gd_wnd = if id == ViewportId::ROOT {
                // Attach directly to this component.
                self.to_gd().add_child(gd_painter.clone().upcast());
                gd_painter.set_owner(self.to_gd().upcast());

                // NOTE: For root viewport...
                //
                // TODO: Merge `IGNORE` behavior between non-root and root.
                // - This is required to implement `add node as child of viewport`
                //   feature.
                //
                // Godot's default `gui_input` handling method, does not propagate inputs
                // into its siblings if they are obscured by this node. Since we're
                // creating a control which covers entire drawable space, and intercepting
                // all inputs, if mouse filter is applied anything other than `IGNORE`
                // would effectively prevent all other non-parent node to receive any
                // input.
                //
                // Therefore, we rather intercept any inputs in `_input()` method, and if
                // we need to consume the input inside egui, we rather make call to
                // `Viewport::set_input_as_handled()` which consumes input even before
                // reaching out to `gui_input()` callbacks of any.
                gd_painter.set_mouse_filter(MouseFilter::IGNORE);

                // To do the tricks
                gd_painter.set_process_input(true);

                None
            } else {
                let builder = &viewport.builder;

                // NOTE: For other viewports, they exclusively use the window, therefore
                // don't need an `input` trick to work correctly.
                gd_painter.set_mouse_filter(MouseFilter::PASS);
                gd_painter.set_process_input(false);

                // Spawn additional window to hold painter.
                let mut gd_wnd = engine::Window::new_alloc();

                self.to_gd().add_child(gd_wnd.clone().upcast());
                gd_wnd.set_owner(self.to_gd().upcast());

                gd_wnd.add_child(gd_painter.clone().upcast());
                gd_painter.set_owner(gd_wnd.clone().upcast());

                // Bind window close request.
                let close_req = viewport.close_request.clone();
                gd_wnd.connect(
                    "close_requested".into(),
                    Callable::from_fn("SubscribeClose", move |_| {
                        close_req.store(VIEWPORT_CLOSE_REQUESTED, Relaxed);
                        Ok(Variant::nil())
                    }),
                );

                // NOTE: List of recreation-only flags
                // - active
                // - app_id
                // - close_button
                // - minimwze_button
                // - maximize_button
                // - title_shown
                // - titlebar_buttons_shown
                // - titlebar_shown
                // - fullsize_content_view
                // - drag_and_drop

                use engine::window::Flags;

                if builder.active.is_some_and(|x| x) {
                    gd_wnd.grab_focus();
                }

                if builder.titlebar_shown.is_some_and(|x| !x) {
                    gd_wnd.set_flag(Flags::BORDERLESS, true);
                }

                Some(gd_wnd)
            };

            *surface = Some(SurfaceContext {
                painter: gd_painter,
                window: gd_wnd,
            });
        }

        let Some(surface) = surface.into_inner() else {
            unreachable!()
        };

        for command in viewport.updates.drain(..) {
            use egui::ViewportCommand::*;

            let Some(mut window) = surface.window.clone() else {
                // Root viewport won't receive any viewport commands.
                continue;
            };

            match command {
                Close => {
                    if id == ViewportId::ROOT {
                        // Ignore close signal to root ... It's simply not allowed!
                        godot_warn!("Root viewport received close request!");
                    } else {
                        // In any other cases; close signal is ignored. User can easily
                        // dispose the viewport by not calling `show_viewport_deferred`
                    }

                    viewport.close_request.store(VIEWPORT_CLOSE_CLOSE, Relaxed);
                }
                CancelClose => {
                    viewport.close_request.store(VIEWPORT_CLOSE_NONE, Relaxed);
                }
                Title(new_title) => {
                    window.set_title(new_title.into());
                }
                Transparent(transparent) => {
                    window.set_transparent_background(transparent);
                }
                Visible(visible) => {
                    window.set_visible(visible);
                }
                StartDrag => {
                    // TODO: Implement this
                    //
                    // Set viewport.dragging = true; then until it finishes dragging, get
                    // mouse delta then move the window.
                }
                OuterPosition(pos) => window.set_position(pos.to_alternative()),

                // FIXME: Change painter size; not the containing window size.
                InnerSize(size) => window.set_size(size.to_alternative()),
                MinInnerSize(size) => window.set_min_size(size.to_alternative()),
                MaxInnerSize(size) => window.set_max_size(size.to_alternative()),
                ResizeIncrements(Some(incr)) => {
                    let size = window.get_size();
                    let new_size = size + incr.to_alternative();
                    window.set_size(new_size);
                }
                ResizeIncrements(None) => {}
                BeginResize(_) => {
                    // TODO: Implement this
                }
                Resizable(value) => window.set_flag(window::Flags::RESIZE_DISABLED, !value),
                EnableButtons { .. } => {}
                Minimized(true) => window.set_mode(window::Mode::MINIMIZED),
                Minimized(_) => {}
                Maximized(true) => window.set_mode(window::Mode::MAXIMIZED),
                Maximized(_) => {}
                Fullscreen(true) => window.set_mode(window::Mode::FULLSCREEN),
                Fullscreen(_) => {}
                Decorations(deco) => window.set_flag(window::Flags::BORDERLESS, !deco),
                WindowLevel(level) => {
                    let enabled = match level {
                        egui::WindowLevel::AlwaysOnBottom | egui::WindowLevel::Normal => false,
                        egui::WindowLevel::AlwaysOnTop => true,
                    };

                    window.set_flag(window::Flags::ALWAYS_ON_TOP, enabled);
                }
                Icon(_) => {
                    // TODO: Find way to handle this.
                }
                IMERect(rect) => {
                    window.set_ime_position(rect.to_alternative().position);
                }
                IMEAllowed(allowed) => {
                    window.set_ime_active(allowed);
                }
                IMEPurpose(_why) => {
                    // TODO: How?
                }
                Focus => {
                    window.grab_focus();
                }
                RequestUserAttention(_) => {
                    // No way?
                }
                SetTheme(_) => {
                    // How?
                }
                ContentProtected(_) => {}
                CursorPosition(_pos) => {}
                CursorGrab(_) => {}
                CursorVisible(_) => {
                    // TODO: How can we achieve this in safe manner?
                    // - e.g. If user simply disposed EGUI after hiding cursor...
                }
                MousePassthrough(enabled) => {
                    window.set_flag(window::Flags::MOUSE_PASSTHROUGH, enabled);
                }
                Screenshot => {
                    // TODO: How?
                }
            }
        }

        if viewport.close_request.load(Relaxed) == VIEWPORT_CLOSE_PENDING {
            // Close request is accepted, so we should dispose this viewport.
            viewport.close_request.store(VIEWPORT_CLOSE_CLOSE, Relaxed);
        }

        // Update viewport input from surface output.
        'wnd: {
            let gd_wnd = match surface.window.clone() {
                Some(wnd) => wnd,
                None => {
                    if let Some(wnd) = surface.painter.get_window() {
                        wnd
                    } else {
                        break 'wnd;
                    }
                }
            };

            let info = &mut viewport.info;

            let inner_pos =
                Vector2::from_vector2i(gd_wnd.get_position()) + surface.painter.get_position();
            let inner_size = surface.painter.get_size();

            let gd_ds = DisplayServer::singleton();
            let id_screen = gd_ds
                .window_get_current_screen_ex()
                .window_id(gd_wnd.get_window_id())
                .done();
            let scale = gd_ds.screen_get_scale_ex().screen(id_screen).done();

            info.inner_rect = Some(Rect2::new(inner_pos, inner_size).to_counterpart());
            info.focused = Some(gd_wnd.has_focus());
            info.native_pixels_per_point = Some(scale);
            info.fullscreen = Some(gd_wnd.get_mode() == window::Mode::FULLSCREEN);
            info.minimized = Some(gd_wnd.get_mode() == window::Mode::MINIMIZED);
            info.maximized = Some(gd_wnd.get_mode() == window::Mode::MAXIMIZED);
            info.monitor_size = Some(
                gd_ds
                    .screen_get_size_ex()
                    .screen(id_screen)
                    .done()
                    .to_counterpart(),
            );
            info.outer_rect = Some(egui::Rect::from_min_size(
                gd_wnd.get_position().to_alternative(),
                gd_wnd.get_size().to_counterpart(),
            ));
        }
        // Just validate viewport information on input
        let input = viewport.info.clone();

        // After copying required information, drop the lock.
        drop(viewport_lock);

        // Reset viewport info.
        self.share
            .raw_input_template
            .lock()
            .viewports
            .insert(id, input);

        // Checkin surface again.
        self.surfaces.borrow_mut().pipe(|mut x| {
            x.entry(id).or_insert(surface);
        });
    }

    fn viewport_start_frame(&self, id: ViewportId) {
        // NOTE: Seems recursive call to begin_frame is handled by stack internally.
        let mut raw_input = self.share.raw_input_template.lock().clone();

        {
            let mut viewport = self.share.viewports.lock();
            let viewport = viewport.get_mut(&id).unwrap();

            raw_input.events.extend(viewport.rx_update.try_iter());
            raw_input.screen_rect = viewport.info.inner_rect.map(|x| {
                egui::Rect::from_min_size(egui::Pos2::ZERO, x.size() / viewport.target_ui_scale)
            });

            raw_input.focused = viewport.info.focused.unwrap_or_default();
            raw_input.viewport_id = id;

            // Just set repaint schedule to far future.
            viewport.repaint_at = Some(Instant::now() + Duration::from_secs(3600));

            // If close request is delivered from platform, forward the event to EGUI that
            // allow user logic to handle this. (e.g. cancel the close request)
            if viewport.close_request.load(Relaxed) == VIEWPORT_CLOSE_REQUESTED {
                viewport
                    .close_request
                    .store(VIEWPORT_CLOSE_PENDING, Relaxed);
                raw_input
                    .viewports
                    .get_mut(&id)
                    .unwrap()
                    .events
                    .push(egui::ViewportEvent::Close);
            }
        }

        self.share.egui.begin_frame(raw_input);
    }

    fn viewport_end_frame(&self, id: ViewportId) {
        // Retrieve viewport-wise output.
        let mut output = self.share.egui.end_frame();

        let paints = take(&mut output.shapes);
        let ppi = output.pixels_per_point;

        // Que async tesselation jobs to Rayon.
        let (tx, rx) = oneshot::channel();

        self.share
            .viewports
            .lock()
            .get_mut(&id)
            .unwrap()
            .pipe(|vp| {
                vp.paint_this_frame = Some(rx);
                vp.target_ui_scale = ppi;
            });

        // Offload tesselation from game thread.
        self.tx_bg_task
            .borrow()
            .as_ref()
            .unwrap()
            .send(BackgroundWorkCommand::Tessellate(paints, ppi, tx))
            .expect("Background task wasn't started.");

        let mut gd_wnd = self
            .surfaces
            .borrow_mut()
            .get(&id)
            .and_then(|x| x.painter.get_window())
            .expect("A painter should be spawned under any valid window!");

        if let Some(ime) = output.platform_output.ime.take() {
            // XXX: Is calling this every frame safe?
            gd_wnd.set_ime_active(true);
            gd_wnd.set_ime_position(ime.cursor_rect.min.to_alternative());
        } else {
            gd_wnd.set_ime_active(false);
        }

        // Handle platform outputs accumulated from all viewports.
        {
            let egui::PlatformOutput {
                open_url,
                copied_text,
                events,
                mutable_text_under_cursor,

                // Handled by each viewport.
                cursor_icon,
                ime: _,
            } = take(&mut output.platform_output);

            let mut ds = DisplayServer::singleton();

            if let Some(url) = open_url {
                open::that(url.url).ok();
            }

            if !copied_text.is_empty() {
                ds.clipboard_set(copied_text.into());
            }

            if mutable_text_under_cursor {
                // XXX: Do we need virtual board ...?
            }

            for _event in events {
                // We're not interested in widget outputs
            }

            let overwrite_cursor = if self.cursor_shape.borrow().is_some() {
                // Do not overwrite meaningful cursor with `None` or `Default`
                !matches!(cursor_icon, CursorIcon::None | CursorIcon::Default)
            } else {
                // Prevent `None` cursor disturbing the engine's cursor control
                cursor_icon != CursorIcon::None
            };

            if overwrite_cursor {
                *self.cursor_shape.borrow_mut() = Some(cursor_icon);
            }
        }

        // Accumulate outputs to primary output.
        self.share.full_output.lock().append(output);

        // Call setup scripts that was queued during frame.
        for script in self.setup_scripts.borrow_mut().drain(..) {
            script(&self.share.egui);
        }
    }

    /// Start frame in thread-safe manner.
    fn queue_try_start_frame(&self) {
        if std::thread::current().id() == self.share.main_thread_id {
            self.try_start_frame();
            return;
        }

        if !self.share.try_advance_frame() {
            return;
        }

        self.to_gd().call_deferred(
            symbol_string!(Self, __internal_try_start_frame_inner).into(),
            &[],
        );
    }
}

impl SharedContext {
    fn repaint(&self, info: egui::RequestRepaintInfo) {
        if let Some(x) = self.viewports.lock().get_mut(&info.viewport_id) {
            x.repaint_at = Some(Instant::now() + info.delay);
        } else {
            godot_warn!("EGUI requested repaint for unregistered viewpot: {info:?}")
        };
    }

    fn try_advance_frame(&self) -> bool {
        !self.frame_started.swap(true, Relaxed)
    }

    fn is_in_frame(&self) -> bool {
        self.frame_started.load(Relaxed)
    }

    fn finish_frame(&self) {
        self.frame_started.store(false, Relaxed);
    }
}
