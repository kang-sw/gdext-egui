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
