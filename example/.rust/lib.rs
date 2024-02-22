use gdext_egui::{egui, ViewportBuilder, ViewportId};
use godot::{engine, prelude::*};

struct MyExtension;

#[gdextension]
unsafe impl ExtensionLibrary for MyExtension {}

#[derive(GodotClass)]
#[class(init, base=Node2D)]
struct Showcase {
    base: Base<Node2D>,

    /// This should be set from editor
    #[export]
    egui: Option<Gd<gdext_egui::EguiBridge>>,

    text_1: String,
    text_2: String,
}

#[godot_api]
impl INode2D for Showcase {
    fn ready(&mut self) {
        let Some(mut vp) = self.base_mut().get_viewport() else {
            godot_error!("Viewport not found");

            return;
        };

        // Let all subwindow has native representation.
        vp.set_embedding_subwindows(false);
    }

    fn process(&mut self, _delta: f64) {
        let Some(egui) = self.egui.clone() else {
            godot_error!("No EGUI node reference set");
            return;
        };

        // This should be called first!
        let egui = egui.bind();
        let ctx = egui.current_frame();

        let time = engine::Time::singleton();
        let tick = time.get_ticks_usec() as f64 / 1e6;

        egui::Window::new("Example Window").show(ctx, |ui| {
            ui.label("hello, world!");
            ui.label(format!("Now: {tick}"));
            ui.text_edit_multiline(&mut self.text_1);
        });

        ctx.show_viewport_immediate(
            ViewportId::from_hash_of("Hah! This is immede"),
            ViewportBuilder::default().with_title("Immeddde~~"),
            move |ctx, _| {
                egui::Window::new("Window in Viewport!").show(ctx, |ui| {
                    ui.label("blah blah");
                    ui.label(format!("Now: {tick}"));
                    ui.text_edit_multiline(&mut self.text_2);
                });
            },
        );

        ctx.show_viewport_deferred(
            ViewportId::from_hash_of("Hah!"),
            ViewportBuilder::default().with_title("Hello~~"),
            move |ctx, _| {
                egui::Window::new("Window in Viewport!").show(ctx, |ui| {
                    ui.label("blah blah");
                    ui.label(format!("Now: {tick}"));
                });
            },
        );
    }
}
