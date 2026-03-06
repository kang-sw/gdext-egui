use std::{
    sync::atomic::{AtomicBool, Ordering},
    time::{Duration, Instant},
};

use godot::prelude::*;

use super::WidgetRetain;

/* ----------------------------------------- Decorators ----------------------------------------- */

/// Base trait for all widget callbacks.
pub trait FnEguiDraw<R>: FnMut(&egui::Context) -> R + 'static
where
    R: Into<WidgetRetain>,
{
}

impl<T, R> FnEguiDraw<R> for T
where
    T: FnMut(&egui::Context) -> R + 'static,
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

impl<T: GodotClass> CheckExpired for Gd<T> {
    fn expired(&self) -> bool {
        self.is_instance_valid()
    }
}

impl CheckExpired for std::rc::Rc<std::cell::Cell<bool>> {
    fn expired(&self) -> bool {
        !self.get()
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
        move |ctx: &egui::Context| {
            if Instant::now() > expiration {
                WidgetRetain::Dispose
            } else {
                self(ctx).into()
            }
        }
    }

    /// Set the expiration time of the widget. If the widget is not disposed after the given
    /// time, it'll be disposed automatically.
    fn bind<C: CheckExpired>(mut self, expired: C) -> impl FnEguiDrawExt<WidgetRetain> {
        move |ctx: &egui::Context| {
            if expired.expired() {
                WidgetRetain::Dispose
            } else {
                self(ctx).into()
            }
        }
    }

    /// Trigger the widget only once. After the first call, the widget will be disposed.
    fn once(mut self) -> impl FnEguiDrawExt<WidgetRetain> {
        move |ctx: &egui::Context| {
            // Only the first call will be executed.
            let _ = self(ctx).into();
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
    T: FnMut(&egui::Context) -> L + 'static,
    L: Into<WidgetRetain> + 'static,
{
}
