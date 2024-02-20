use godot::{engine, prelude::*};

#[derive(GodotClass)]
#[class(base=CanvasLayer, init, rename=GodotEguiBridge)]
pub struct EguiBridge {
    base: Base<engine::CanvasLayer>,
}
