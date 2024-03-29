use gdext_egui::{egui, EguiBridge, ViewportBuilder, ViewportId};
use godot::{
    engine::{EditorInterface, EditorPlugin, IEditorPlugin},
    prelude::*,
};

#[derive(GodotClass)]
#[class(base = EditorPlugin, editor_plugin, tool, init)]
pub struct TestEguiPlugin {
    base: Base<EditorPlugin>,
    egui: Option<Gd<EguiBridge>>,
}

#[godot_api]
impl IEditorPlugin for TestEguiPlugin {
    fn enter_tree(&mut self) {
        if let Some(mut prev) = self.egui.replace(EguiBridge::new_alloc()) {
            prev.queue_free();
        }

        let egui = self.egui.as_ref().unwrap();

        egui.bind().setup_context(|ctx| {
            ctx.set_zoom_factor(1.5);
        });

        let edt = EditorInterface::singleton();
        let mut main_screen = edt.get_editor_main_screen().unwrap();
        main_screen.add_child(egui.clone().upcast());

        egui.bind().sync_root_region(Some(main_screen.upcast()));
        self.make_visible(false);
    }

    fn exit_tree(&mut self) {
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

    fn process(&mut self, _dt: f64) {
        let Some(egui) = self.egui.as_ref() else {
            return;
        };

        if !egui.is_visible() {
            return;
        }

        let ctx = egui.bind().current_frame().clone();

        egui::Window::new("aglagfSD").show(&ctx, |ui| {
            ui.label("Hello World!");
        });
    }
}
