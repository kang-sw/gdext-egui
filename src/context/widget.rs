//! Widget related APIs. Detached due to verbosity.

use std::{
    collections::BTreeMap,
    sync::atomic::{AtomicBool, Ordering},
};

use super::EguiBridge;

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
        mut widget: impl FnMut(&mut egui::Ui) -> L + 'static,
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
    pub fn panel_item_insert<L>(
        &self,
        panel: PanelGroup,
        slot: i32,
        widget: impl FnMut(&mut egui::Ui) -> L + 'static,
    ) where
        L: Into<WidgetRetain>,
    {
        assert!((i32::MIN >> 1..=i32::MAX >> 1).contains(&slot));
        self.impl_push_panel_item(panel, NewWidgetSlot::Specified(slot), widget);
    }

    /// See [`EguiBridge::panel_item_insert`].
    pub fn panel_item_push_back<L>(
        &self,
        panel: PanelGroup,
        widget: impl FnMut(&mut egui::Ui) -> L + 'static,
    ) where
        L: Into<WidgetRetain>,
    {
        self.impl_push_panel_item(panel, NewWidgetSlot::Append, widget);
    }

    /// See [`EguiBridge::panel_item_insert`].
    pub fn panel_item_push_front<L>(
        &self,
        panel: PanelGroup,
        widget: impl FnMut(&mut egui::Ui) -> L + 'static,
    ) where
        L: Into<WidgetRetain>,
    {
        self.impl_push_panel_item(panel, NewWidgetSlot::Prepend, widget);
    }

    fn impl_push_panel_item<L>(
        &self,
        panel: PanelGroup,
        slot: NewWidgetSlot,
        mut widget: impl FnMut(&mut egui::Ui) -> L + 'static,
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

            widgets.insert(
                (group, slot_index),
                PanelItem {
                    draw,
                    retain: Default::default(),
                },
            );
        }

        // Based on layout; draw widgets
        let enums = [
            PanelGroup::Left,
            PanelGroup::Right,
            PanelGroup::Central,
            PanelGroup::BottomLeft,
            PanelGroup::BottomRight,
        ];

        let [has_left, has_right, has_central, has_bottom_left, has_bottom_right] =
            enums.map(|x| widgets.range_mut(x.range()).any(|_| true));

        fn draw_fn(
            ui: &mut egui::Ui,
            widgets: &mut BTreeMap<(PanelGroup, i32), Box<FnShowWidget>>,
            group: PanelGroup,
        ) {
            // TODO: Draw widgets
        }

        // Draw top side of panels

        let draw_top = |ui: &mut egui::Ui| {
            // TODO: Split sections if required
        };

        if has_left || has_right || has_central {
            if has_bottom_left || has_bottom_right {
                egui::TopBottomPanel::top("__RootBridge::TopBottom").show(ctx, draw_top);
            } else {
                egui::CentralPanel::default().show(ctx, draw_top);
            }
        }

        // Draw bottom side of panels
        if has_bottom_left || has_bottom_right {
            // Always fill the rest of panel
            egui::CentralPanel::default().show(ctx, |ui| {
                // TODO: Draw bottom side of panels
            });
        }

        // TODO: Remove disposed widgets
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
    retain: WidgetRetain,
}

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

impl<T> From<std::rc::Weak<T>> for WidgetRetain {
    fn from(x: std::rc::Weak<T>) -> Self {
        if x.strong_count() > 0 {
            Self::Retain
        } else {
            Self::Dispose
        }
    }
}

impl<T> From<std::sync::Weak<T>> for WidgetRetain {
    fn from(x: std::sync::Weak<T>) -> Self {
        if x.strong_count() > 0 {
            Self::Retain
        } else {
            Self::Dispose
        }
    }
}

impl From<std::sync::Arc<AtomicBool>> for WidgetRetain {
    fn from(x: std::sync::Arc<AtomicBool>) -> Self {
        if x.load(Ordering::Relaxed) {
            Self::Retain
        } else {
            Self::Dispose
        }
    }
}

impl From<std::rc::Rc<bool>> for WidgetRetain {
    fn from(x: std::rc::Rc<bool>) -> Self {
        if *x {
            Self::Retain
        } else {
            Self::Dispose
        }
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
