macro_rules! symbol_string {
    ($base:ty, $ident:ident) => {{
        // Just ensure ident exist
        let str = stringify!($ident);
        let _ = <$base>::$ident;

        str
    }};
}

pub mod context;
mod surface;

pub extern crate egui;

pub use context::EguiBridge;
pub use egui::{ViewportBuilder, ViewportId};

pub mod helpers {
    use godot::{classes::WeakRef, prelude::*};

    pub trait ToCounterpart {
        type Counterpart;
        type Alternative;

        fn to_counterpart(&self) -> Self::Counterpart;
        fn to_alternative(&self) -> Self::Alternative;
    }

    impl ToCounterpart for Vector2 {
        type Counterpart = egui::Vec2;
        type Alternative = egui::Pos2;

        fn to_counterpart(&self) -> Self::Counterpart {
            egui::Vec2::new(self.x, self.y)
        }

        fn to_alternative(&self) -> Self::Alternative {
            egui::Pos2::new(self.x, self.y)
        }
    }

    impl ToCounterpart for egui::Vec2 {
        type Counterpart = Vector2;
        type Alternative = Vector2i;

        fn to_counterpart(&self) -> Self::Counterpart {
            Vector2::new(self.x, self.y)
        }

        fn to_alternative(&self) -> Self::Alternative {
            Vector2i::new(self.x as i32, self.y as i32)
        }
    }

    impl ToCounterpart for egui::Pos2 {
        type Counterpart = Vector2;
        type Alternative = Vector2i;

        fn to_counterpart(&self) -> Self::Counterpart {
            Vector2::new(self.x, self.y)
        }

        fn to_alternative(&self) -> Self::Alternative {
            Vector2i::new(self.x as i32, self.y as i32)
        }
    }

    impl ToCounterpart for Vector2i {
        type Counterpart = egui::Vec2;
        type Alternative = egui::Pos2;

        fn to_counterpart(&self) -> Self::Counterpart {
            egui::Vec2::new(self.x as f32, self.y as f32)
        }

        fn to_alternative(&self) -> Self::Alternative {
            egui::Pos2::new(self.x as f32, self.y as f32)
        }
    }

    impl ToCounterpart for Rect2 {
        type Counterpart = egui::Rect;
        type Alternative = Self::Counterpart;

        fn to_counterpart(&self) -> Self::Counterpart {
            egui::Rect::from_min_size(
                self.position.to_counterpart().to_pos2(),
                self.size.to_counterpart(),
            )
        }

        fn to_alternative(&self) -> Self::Alternative {
            self.to_counterpart()
        }
    }

    impl ToCounterpart for egui::Rect {
        type Counterpart = Rect2;
        type Alternative = Rect2i;

        fn to_counterpart(&self) -> Self::Counterpart {
            Rect2::new(self.min.to_counterpart(), self.max.to_counterpart())
        }

        fn to_alternative(&self) -> Self::Alternative {
            Rect2i::new(self.min.to_alternative(), self.max.to_alternative())
        }
    }

    pub fn downgrade_gd<T: GodotClass>(gd: Gd<T>) -> Gd<WeakRef> {
        godot::global::weakref(&gd.to_variant()).try_to().unwrap()
    }

    pub fn try_upgrade_gd<T: GodotClass>(gd: Gd<WeakRef>) -> Option<Gd<T>> {
        gd.get_ref().try_to().ok()
    }
}

fn default<T: Default>() -> T {
    T::default()
}

/* ------------------------------------------ DnD Util ------------------------------------------ */

/// A wrapper around `godot::prelude::Variant` that forcibly implements `Send` and `Sync`.
///
/// This is the primary `Payload` type for [`egui::DragAndDrop`] utility. This is not
/// intended to use carry 'heavy' data, but rather to carry references to the data which
/// is relatively cheap to clone.
#[derive(Debug, Clone)]
pub struct DragAndDropVariant(godot::prelude::Variant);

unsafe impl Send for DragAndDropVariant {}
unsafe impl Sync for DragAndDropVariant {}

impl DragAndDropVariant {
    pub fn new(variant: godot::prelude::Variant) -> Self {
        Self(variant)
    }

    pub fn into_inner(self) -> godot::prelude::Variant {
        self.0
    }
}
