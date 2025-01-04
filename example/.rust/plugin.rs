use std::{cell::Cell, rc::Rc};

use gdext_egui::{context::FnEguiDrawExt, egui, EguiBridge};
use godot::{
    classes::{EditorInterface, EditorPlugin, IEditorPlugin},
    prelude::*,
};

#[derive(GodotClass)]
#[class(base = EditorPlugin, tool, init)]
pub struct TestEguiPlugin {
    base: Base<EditorPlugin>,
    egui: Option<Gd<EguiBridge>>,

    handle: Option<Rc<()>>,
    counter: Cell<usize>,
}

#[godot_api]
impl IEditorPlugin for TestEguiPlugin {
    fn ready(&mut self) {
        if let Some(mut prev) = self.egui.replace(EguiBridge::new_alloc()) {
            prev.queue_free();
        }

        let egui = self.egui.as_ref().unwrap();

        egui.bind().setup_context(|ctx| {
            ctx.set_zoom_factor(1.5);
        });

        let handle = Rc::new(());
        let self_gd = self.to_gd();
        egui.bind().register_render_callback_last(
            0,
            (move |ctx: &egui::Context| {
                self_gd.bind().show(ctx);
            })
            .bind(Rc::downgrade(&handle)),
        );
        self.handle = Some(handle);

        let edt = EditorInterface::singleton();
        let mut main_screen = edt.get_editor_main_screen().unwrap();
        main_screen.add_child(egui);

        egui.bind().sync_root_region(Some(main_screen.upcast()));
        self.make_visible(false);
    }

    fn exit_tree(&mut self) {
        let _ = self.handle.take();

        if let Some(mut egui) = self.egui.take() {
            egui.queue_free();
        }
    }

    fn has_main_screen(&self) -> bool {
        true
    }

    fn get_plugin_name(&self) -> GString {
        "Test Egui Plugin".into()
    }

    fn make_visible(&mut self, value: bool) {
        let Some(egui) = self.egui.as_mut() else {
            return;
        };

        egui.set_visible(value);
    }

    fn process(&mut self, _: f64) {
        let Some(egui) = self.egui.as_ref() else {
            return;
        };

        let _ = egui;
    }
}

impl TestEguiPlugin {
    fn show(&self, ctx: &egui::Context) {
        egui::Window::new("aglagfSD").show(ctx, |ui| {
            self.counter.set(self.counter.get() + 1);
            ui.label(format!("Hello World! {}", self.counter.get()));
        });
    }
}
