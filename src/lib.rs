#[macro_export]
macro_rules! string_name {
    ($base:ty, $ident:ident) => {{
        // Just ensure ident exist
        let str = $crate::string_name!(#[str], $ident);
        let _ = <$base>::$ident;

        $crate::string_name!(#[set], str)
    }};

    (#[str], $id:expr) => {
        concat!(stringify!($ident), "\0").as_bytes()
    };

    (#[set], $bytes:expr) => {
        // XXX: Optimization
        // - Is it cheap enough to create everytime?
        // - Otherwise, is it safe to store in OnceLock?
        godot::prelude::StringName::from_latin1_with_nul($bytes)
    };
}

pub mod context;
mod painter;

pub extern crate egui;

pub use context::EguiBridge;
pub use egui::{ViewportBuilder, ViewportId};

fn default<T: Default>() -> T {
    T::default()
}

pub mod helpers {
    use godot::prelude::*;

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
}
