mod frame;
mod viewport;
mod widget_traits;

pub use widget_traits::*;

use std::{
    cell::{Cell, RefCell},
    sync::{
        atomic::{AtomicBool, AtomicU8, Ordering::Relaxed},
        mpsc, Arc,
    },
    thread::ThreadId,
    time::Instant,
};

use educe::Educe;
use egui::{
    mutex::Mutex, DeferredViewportUiCallback, ViewportBuilder, ViewportClass, ViewportId,
    ViewportIdMap,
};
use godot::{
    classes::{self, CanvasLayer, Control, ICanvasLayer, WeakRef},
    prelude::*,
};
use tap::prelude::Pipe;

use crate::{helpers::downgrade_gd, surface};

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
    #[init(val = 13)]
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

    /// Handle to tesselation worker tx channel
    tx_bg_task: RefCell<Option<mpsc::Sender<DeferredCommand>>>,
    rx_bg_task: RefCell<Option<mpsc::Receiver<DeferredCommand>>>,

    /// A object that root region is being synced.
    root_region_sync: Cell<Option<Gd<WeakRef>>>,

    /// callbacks for widgets rendering.
    widget_callbacks_first: RefCell<Vec<(i32, Box<FnWidgetCallback>)>>,
    widget_callbacks_last: RefCell<Vec<(i32, Box<FnWidgetCallback>)>>,

    /// non-send + non-sync even when threading is implemented for godot objects ...
    _non_send_sync: std::marker::PhantomData<*const ()>,
}

type FnWidgetCallback = dyn FnMut(&egui::Context) -> WidgetRetain + 'static;

pub(crate) enum DeferredCommand {
    RequestRepaint(ViewportId),
}

#[derive(Clone)]
pub(crate) struct SurfaceContext {
    /// Actual painter window.
    painter: Gd<surface::EguiViewportBridge>,

    /// Container window if exist.
    window: Option<Gd<classes::Window>>,
}

#[derive(Educe)]
#[educe(Default)]
pub(crate) struct SharedContext {
    pub(crate) egui: egui::Context,

    /// Repaint was queued.
    pub(crate) repaint_queued: AtomicBool,

    /// Detects whether to start new frame.
    frame_started: AtomicBool,

    /// Template input for each viewport rendering.
    pub(crate) raw_input_template: Mutex<egui::RawInput>,

    /// Accumulated output for entire single frame.
    pub(crate) full_output: Mutex<egui::FullOutput>,

    /// List of viewports that is tracked by this context.
    pub(crate) spawned_viewports: Mutex<ViewportIdMap<SpawnedViewportContext>>,

    /// List of viewports that
    pub(crate) viewports: Mutex<ViewportIdMap<ViewportContext>>,

    /// The thread ID that instance was initiated.
    #[educe(Default = std::thread::current().id())]
    pub(crate) main_thread_id: ThreadId,
}

pub(crate) struct SpawnedViewportContext {
    /// Captures `dispose`, then set it to false when viewport closed.
    pub(crate) repaint: Arc<DeferredViewportUiCallback>,

    /// Should spawned viewport be closed?
    pub(crate) dispose: Arc<Mutex<WidgetRetain>>,

    /// Sets at the very first frame.
    pub(crate) builder: egui::ViewportBuilder,
}

pub(crate) struct ViewportContext {
    /// Repainted when time point reaches here.
    pub(crate) repaint_at: Option<Instant>,

    /// Any input captures from viewport.
    pub(crate) rx_update: mpsc::Receiver<egui::Event>,

    /// Viewport initialization
    pub(crate) builder: egui::ViewportBuilder,

    /// Close request status
    pub(crate) close_request: Arc<ViewportClose>,

    /// Viewport commands pending apply. When should be recreated, the second parameter
    /// set to [`Some`].
    pub(crate) updates: Vec<egui::ViewportCommand>,

    /// Paint commands that is being applied,
    pub(crate) paint_this_frame: Option<Vec<egui::ClippedPrimitive>>,

    /// Logical zoom rate requested by user. The painter will accept and rescale event
    /// position and paintings to fit the zoom rate.
    pub(crate) target_ui_scale: f32,

    /// Cached viewport information, that we're currently updating on.
    pub(crate) info: egui::ViewportInfo,
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

pub(crate) const VIEWPORT_CLOSE_NONE: u8 = 0;
pub(crate) const VIEWPORT_CLOSE_REQUESTED: u8 = 1;
pub(crate) const VIEWPORT_CLOSE_PENDING: u8 = 2;
pub(crate) const VIEWPORT_CLOSE_CLOSE: u8 = 3;

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
        self.handle_bg_message();

        if self.share.repaint_queued.swap(false, Relaxed) {
            self.current_frame();
        }

        if self.share.is_in_frame() {
            self.finish_frame();
        }

        self.handle_bg_message();
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
        show: impl FnMut(&egui::Context, ViewportClass) -> R,
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

    /// Registers callback for widget rendering at frame start.
    ///
    /// See also [`FnEguiDrawExt`] decorator for every method with signature
    /// `impl FnMut(&egui::Context) -> impl Into<WidgetRetain> + 'static`
    ///
    /// Callbacks registered with lower priority will be called earlier.
    pub fn register_render_callback_first<L>(&self, priority: i32, widget: impl FnEguiDraw<L>)
    where
        L: Into<WidgetRetain>,
    {
        self.impl_push_panel_item(true, priority, widget);
    }

    /// Registers callback for widget rendering at frame end.
    ///
    /// See also [`FnEguiDrawExt`] decorator for every method with signature
    /// `impl FnMut(&egui::Context) -> impl Into<WidgetRetain> + 'static`
    ///
    /// Callbacks registered with lower priority will be called earlier.
    pub fn register_render_callback_last<L>(&self, priority: i32, widget: impl FnEguiDraw<L>)
    where
        L: Into<WidgetRetain>,
    {
        self.impl_push_panel_item(false, priority, widget);
    }

    /// Registers callback for widget rendering, at very first of the frame start.
    fn impl_push_panel_item<L>(&self, first: bool, priority: i32, mut widget: impl FnEguiDraw<L>)
    where
        L: Into<WidgetRetain>,
    {
        let show = Box::new(move |ui: &_| widget(ui).into());
        let mut arr = if first {
            self.widget_callbacks_first.borrow_mut()
        } else {
            self.widget_callbacks_last.borrow_mut()
        };

        let insert_index = arr
            .binary_search_by_key(&priority, |(p, ..)| *p)
            .unwrap_or_else(|x| x);

        arr.insert(insert_index, (priority, show));
        self.share.repaint_queued.store(true, Relaxed);
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

/* ------------------------------------------ Internals ----------------------------------------- */

impl SharedContext {
    pub(crate) fn repaint(&self, info: egui::RequestRepaintInfo) {
        if let Some(x) = self.viewports.lock().get_mut(&info.viewport_id) {
            x.repaint_at = Some(Instant::now() + info.delay);
            self.repaint_queued.store(true, Relaxed);
        } else {
            godot_warn!("EGUI requested repaint for unregistered viewpot: {info:?}")
        };
    }

    pub(crate) fn try_advance_frame(&self) -> bool {
        !self.frame_started.swap(true, Relaxed)
    }

    pub(crate) fn is_in_frame(&self) -> bool {
        self.frame_started.load(Relaxed)
    }

    pub(crate) fn finish_frame(&self) {
        self.frame_started.store(false, Relaxed);
    }
}
