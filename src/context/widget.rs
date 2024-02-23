//! Widget related APIs. Detached due to verbosity.

use std::{
    sync::atomic::{AtomicBool, Ordering},
    time::{Duration, Instant},
};

use super::EguiBridge;

/* ---------------------------------------------------------------------------------------------- */
/*                                          PUBLIC TYPES                                          */
/* ---------------------------------------------------------------------------------------------- */

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

/* ------------------------------------- Panel Grouping ------------------------------------- */

/// There are several predefined panels that can be used as a root of the viewport.
///
/// These are lazily created if there's any widget that you have added any widget on that
/// panel group.
///
/// ## Layout
///
/// ```text
///         ┌──────────────────────────────────────┐
///         │add_menu                              │
///         ├─────────┬─────────────────┬──────────┤
///         │         │                 │          │
///         │ Left    │ Central         │ Right    │
///         │         │                 │          │
///         ├─────────┴─────────┬───────┴──────────┤
///         │                   │                  │
///         │ BottomLeft        │ BottomRight      │
///         │                   │                  │
///         └───────────────────┴──────────────────┘
/// ```
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[non_exhaustive]
pub enum PanelGroup {
    #[default]
    Left,
    Right,
    Central,
    BottomRight,
    BottomLeft,
}

impl PanelGroup {
    pub fn range(&self) -> std::ops::RangeInclusive<(Self, i32)> {
        (*self, i32::MIN)..=(*self, i32::MAX)
    }
}

/* ----------------------------------------- Decorators ----------------------------------------- */

/// Base trait for all widget callbacks.
pub trait FnEguiDraw<R>: FnMut(&mut egui::Ui) -> R + 'static
where
    R: Into<WidgetRetain>,
{
}

impl<T, R> FnEguiDraw<R> for T
where
    T: FnMut(&mut egui::Ui) -> R + 'static,
    R: Into<WidgetRetain> + 'static,
{
}

/* ------------------------------------- Expiration Sentinel ------------------------------------ */

pub trait CheckExpired: 'static {
    fn expired(&self) -> bool;
}

impl<T: 'static> CheckExpired for std::rc::Weak<T> {
    fn expired(&self) -> bool {
        self.strong_count() == 0
    }
}

impl<T: 'static> CheckExpired for std::sync::Weak<T> {
    fn expired(&self) -> bool {
        self.strong_count() == 0
    }
}

impl CheckExpired for std::sync::Arc<AtomicBool> {
    fn expired(&self) -> bool {
        !self.load(Ordering::Relaxed)
    }
}

impl CheckExpired for std::rc::Rc<std::cell::Cell<bool>> {
    fn expired(&self) -> bool {
        !self.get()
    }
}

impl CheckExpired for godot::engine::WeakRef {
    fn expired(&self) -> bool {
        self.get_ref().is_nil()
    }
}

impl CheckExpired for () {
    fn expired(&self) -> bool {
        false
    }
}

impl CheckExpired for bool {
    fn expired(&self) -> bool {
        !*self
    }
}

/* ------------------------------------------ Extension ----------------------------------------- */

/// Various utilities to extend the widget callback.
pub trait FnEguiDrawExt<L: Into<WidgetRetain>>: Sized + FnEguiDraw<L> {
    /// Set the expiration time of the widget. If the widget is not disposed after the given
    /// system time, it'll be disposed automatically.
    fn expires_at(mut self, expiration: Instant) -> impl FnEguiDrawExt<WidgetRetain> {
        move |ui: &mut egui::Ui| {
            if Instant::now() > expiration {
                WidgetRetain::Dispose
            } else {
                self(ui).into()
            }
        }
    }

    /// Set the expiration time of the widget. If the widget is not disposed after the given
    /// time, it'll be disposed automatically.
    fn bind<C: CheckExpired>(mut self, owner: impl Into<C>) -> impl FnEguiDrawExt<WidgetRetain> {
        let expired = owner.into();
        move |ui: &mut egui::Ui| {
            if expired.expired() {
                WidgetRetain::Dispose
            } else {
                self(ui).into()
            }
        }
    }

    /// Trigger the widget only once. After the first call, the widget will be disposed.
    fn once(mut self) -> impl FnEguiDrawExt<WidgetRetain> {
        move |ui: &mut egui::Ui| {
            // Only the first call will be executed.
            let _ = self(ui).into();
            WidgetRetain::Dispose
        }
    }

    /// Set the lifespan of the widget. If the widget is not disposed after the given
    /// time, it'll be disposed automatically.
    ///
    /// # Warning
    ///
    /// The time is not game delta time, but the system time: Which means, even if you
    /// stopped the game, the widget will be disposed after the given 'real' time.
    fn lifespan(self, duration: Duration) -> impl FnEguiDrawExt<WidgetRetain> {
        self.expires_at(Instant::now() + duration)
    }
}

impl<T, L> FnEguiDrawExt<L> for T
where
    T: FnMut(&mut egui::Ui) -> L + 'static,
    L: Into<WidgetRetain> + 'static,
{
}

/* ---------------------------------------------------------------------------------------------- */
/*                                              APIS                                              */
/* ---------------------------------------------------------------------------------------------- */

impl EguiBridge {
    /// Add a widget item to the main menu bar. This will silently replace the existing
    /// item if path is already exist.
    ///
    /// # Usage
    ///
    /// ```no_run
    /// # let egui = EguiBridge::new_alloc();
    ///
    /// egui.add_menu_item(["File", "New"], |ui| {
    ///     if ui.button("Empty").clicked() {
    ///         // ...
    ///     }
    /// });
    /// ```
    ///
    /// # Panics
    ///
    /// Spawning another menu item inside widget callback is not allowed. (This behavior
    /// can be changed in the future)
    pub fn menu_item_insert<T, L>(
        &self,
        path: impl IntoIterator<Item = T>,
        mut widget: impl FnEguiDraw<L>,
    ) where
        T: Into<String>,
        L: Into<WidgetRetain>,
    {
        let mut node = &mut *self.widget_menu_items.borrow_mut();
        for seg in path {
            let seg = seg.into();
            node = node.children.entry(seg).or_default();
        }

        let show = Box::new(move |ui: &mut _| widget(ui).into());
        node.draw.replace(show);
    }

    /// Add a widget item to specified panel group with given order. It'll silently
    /// replace the existing item if path is already exist.
    ///
    /// For detailed information of predefined panels, see [`PanelGroup`].
    ///
    /// # Panics
    ///
    /// Slot range exceeds i32::MAX >> 1 or i32::MIN >> 1.
    pub fn panel_item_insert<L>(&self, panel: PanelGroup, slot: i32, widget: impl FnEguiDraw<L>)
    where
        L: Into<WidgetRetain>,
    {
        assert!((i32::MIN >> 1..=i32::MAX >> 1).contains(&slot));
        self.impl_push_panel_item(panel, NewWidgetSlot::Specified(slot), widget);
    }

    /// See [`EguiBridge::panel_item_insert`].
    pub fn panel_item_push_back<L>(&self, panel: PanelGroup, widget: impl FnEguiDraw<L>)
    where
        L: Into<WidgetRetain>,
    {
        self.impl_push_panel_item(panel, NewWidgetSlot::Append, widget);
    }

    /// See [`EguiBridge::panel_item_insert`].
    pub fn panel_item_push_front<L>(&self, panel: PanelGroup, widget: impl FnEguiDraw<L>)
    where
        L: Into<WidgetRetain>,
    {
        self.impl_push_panel_item(panel, NewWidgetSlot::Prepend, widget);
    }

    fn impl_push_panel_item<L>(
        &self,
        panel: PanelGroup,
        slot: NewWidgetSlot,
        mut widget: impl FnEguiDraw<L>,
    ) where
        L: Into<WidgetRetain>,
    {
        let show = Box::new(move |ui: &mut _| widget(ui).into());
        self.widgets_new.borrow_mut().push(((panel, slot), show));
    }
}

/* ------------------------------------------ Internals ----------------------------------------- */

impl EguiBridge {
    pub(super) fn _finish_frame_render_spawned_main_menu(&self) {
        // TODO
    }

    pub(super) fn _finish_frame_render_spawned_panels(&self) {
        // Apply widget patches right before rendering.
        let ctx = &self.share.egui;
        let widgets = &mut *self.widgets.borrow_mut();

        const APPEND_SLOT_LOWER: i32 = i32::MAX >> 1;
        const PREPEND_SLOT_UPPER: i32 = i32::MIN >> 1;

        /* --------------------------------- New Slot Allocation -------------------------------- */

        for ((group, slot), draw) in self.widgets_new.borrow_mut().drain(..) {
            let slot_index = match slot {
                NewWidgetSlot::Specified(idx) => idx,
                NewWidgetSlot::Append => {
                    let idx = widgets
                        .range(group.range())
                        .last() // For btree range, it's fast.
                        .map(|((_, idx), ..)| *idx + 1)
                        .unwrap_or_default()
                        .max(APPEND_SLOT_LOWER);

                    idx
                }
                NewWidgetSlot::Prepend => {
                    let idx = widgets
                        .range(group.range())
                        .next()
                        .map(|((_, idx), ..)| *idx - 1)
                        .unwrap_or_default()
                        .min(PREPEND_SLOT_UPPER);

                    idx
                }
            };

            widgets.insert((group, slot_index), PanelItem { draw });
        }

        // Based on layout; draw widgets
        let enums = [
            PanelGroup::Left,
            PanelGroup::Right,
            PanelGroup::Central,
            PanelGroup::BottomLeft,
            PanelGroup::BottomRight,
        ];

        let [has_left, has_right, has_center, has_bottom_left, has_bottom_right] =
            enums.map(|x| widgets.range_mut(x.range()).any(|_| true));
        let has_top = has_left || has_right || has_center;
        let has_bottom = has_bottom_left || has_bottom_right;

        let mut disposed = Vec::new();

        macro_rules! draw_group {
            ($ui:expr, $panel:expr) => {
                for (index, item) in widgets.range_mut($panel.range()) {
                    let retain = (item.draw)($ui);

                    if retain == WidgetRetain::Dispose {
                        disposed.push(*index);
                    }
                }
            };
        }

        // Draw transparent frame to fill the empty space.
        let transparent = egui::Frame::default().fill(egui::Color32::from_black_alpha(0));
        let opaque = egui::Frame::default().fill(egui::Color32::from_black_alpha(71));

        // Draw bottom side of panels first; let top side expand as much as possible.
        if has_bottom {
            egui::TopBottomPanel::bottom("%%EguiBridge%%PanelBottom")
                .frame(opaque)
                .show(ctx, |ui| match (has_bottom_left, has_bottom_right) {
                    (true, true) => {
                        ui.columns(2, |col| {
                            draw_group!(&mut col[0], PanelGroup::BottomLeft);
                            draw_group!(&mut col[1], PanelGroup::BottomRight);
                        });
                    }
                    (true, false) => draw_group!(ui, PanelGroup::BottomLeft),
                    (false, true) => draw_group!(ui, PanelGroup::BottomRight),
                    (false, false) => unreachable!(),
                });
        }

        if has_top {
            if has_left {
                egui::SidePanel::left("%%EguiBridge%%PanelLeft")
                    .frame(opaque)
                    .show(ctx, |ui| draw_group!(ui, PanelGroup::Left));
            }

            if has_right {
                egui::SidePanel::right("%%EguiBridge%%PanelRight")
                    .frame(opaque)
                    .show(ctx, |ui| draw_group!(ui, PanelGroup::Right));
            }

            if has_center {
                // To allow clicks on the empty space, here we create window with
                // transparent frame.

                egui::Window::new("%%EguiBridge%%PanelCentral")
                    .movable(false)
                    .auto_sized()
                    .anchor(egui::Align2::LEFT_TOP, egui::Vec2::ZERO)
                    .frame(transparent)
                    .title_bar(false)
                    .show(ctx, |ui| {
                        draw_group!(ui, PanelGroup::Central);
                    });
            }
        }

        // Gc removed entries.
        for index in disposed {
            assert!(widgets.remove(&index).is_some());
        }
    }
}

/* ---------------------------------------------------------------------------------------------- */
/*                                              TYPES                                             */
/* ---------------------------------------------------------------------------------------------- */

/* ------------------------------------- Widget Creation ------------------------------------ */

/// Type alias for widget declaration
pub(super) type NewWidgetItem = ((PanelGroup, NewWidgetSlot), Box<FnShowWidget>);

/// Callback for showing spawned widget.
pub(super) type FnShowWidget = dyn FnMut(&mut egui::Ui) -> WidgetRetain + 'static;

pub(super) enum NewWidgetSlot {
    Specified(i32),
    Append,
    Prepend,
}

/// Widget declaration & context
pub(super) struct PanelItem {
    draw: Box<FnShowWidget>,
}
