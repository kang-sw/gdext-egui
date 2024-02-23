use std::sync::{
    atomic::{AtomicUsize, Ordering::Relaxed},
    Arc,
};

use gdext_egui::{context::PanelGroup, egui, ViewportBuilder, ViewportId};
use godot::{
    engine::{self, CanvasLayer, ICanvasLayer},
    prelude::*,
};
struct MyExtension;

#[gdextension]
unsafe impl ExtensionLibrary for MyExtension {}

/* ---------------------------------------------------------------------------------------------- */
/*                                            SHOWCASE                                            */
/* ---------------------------------------------------------------------------------------------- */

#[derive(GodotClass)]
#[class(init, base=Node)]
struct Showcase {
    base: Base<Node>,

    /// This should be set from editor
    #[init(default = OnReady::manual())]
    egui: OnReady<Gd<gdext_egui::EguiBridge>>,

    demos: egui_demo_lib::DemoWindows,
}

#[godot_api]
impl INode for Showcase {
    fn process(&mut self, _d: f64) {
        // Actually this is all you need.
        let ctx = self.egui.bind().current_frame().clone();
        self.demos.ui(&ctx);
    }

    fn ready(&mut self) {
        self.egui.init(gdext_egui::EguiBridge::new_alloc());

        let mut gd_self = self.to_gd();
        gd_self.add_child(self.egui.clone().upcast());
        self.egui.set_owner(gd_self.upcast());

        // Let all subwindow has native representation.
        self.base()
            .get_viewport()
            .unwrap()
            .set_embedding_subwindows(false);

        /* ---------------------------------- Spawning Examples --------------------------------- */

        let egui = self.egui.bind();

        // You can spawn viewport
        egui.viewport_spawn(
            ViewportId::from_hash_of(31),
            ViewportBuilder::default().with_title("Demo Viewport"),
            {
                let mut demo = egui_demo_lib::ColorTest::default();
                move |ctx| {
                    egui::CentralPanel::default().show(ctx, |ui| {
                        demo.ui(ui);
                    });

                    // Within callback:
                    // - Return () => Disposed when the viewport window is closed.
                    // - Return true => Keep the viewport window open.
                    // - Return false => Dispose the viewport window immediately.
                }
            },
        );

        // Menu items can be spawned
        egui.menu_item_insert(["Menu Example"], {
            let mut hidden = false;
            move |ui| {
                if ui.button("Click to Hide Me!").clicked() {
                    hidden = !hidden;
                }

                !hidden
            }
        });

        // There are several pre-defined panels in EguiBridge. Unless you use this
        // functionality, they don't affect any of your EGUI context. These are useful
        // when you don't want to write any *central* context your own which manages the
        // layout and appearance of the individual UI items.
        let spawn_group_item = |group: PanelGroup, index: i32| {
            egui.panel_item_insert(group, index, move |ui| {
                ui.label(format!("{:?} Panel: {}", group, index));
            });
        };

        spawn_group_item(PanelGroup::Left, 0);
        spawn_group_item(PanelGroup::Left, 1);
        spawn_group_item(PanelGroup::Right, 0);
        spawn_group_item(PanelGroup::Right, 1);
        spawn_group_item(PanelGroup::Central, -1);
        spawn_group_item(PanelGroup::Central, 3);
        spawn_group_item(PanelGroup::Central, 2);
    }
}

/* ---------------------------------------------------------------------------------------------- */
/*                                            TOOL TEST                                           */
/* ---------------------------------------------------------------------------------------------- */

/// With this node, as soon as you open the scene that this node is included, it'll start
/// showing the UI.
#[derive(GodotClass)]
#[class(tool, init, base=CanvasLayer)]
struct ToolTest {
    base: Base<CanvasLayer>,

    /// This should be set from editor
    #[export]
    egui: Option<Gd<gdext_egui::EguiBridge>>,

    text_1: String,
    text_2: String,

    count: Arc<AtomicUsize>,
}

#[godot_api]
impl ICanvasLayer for ToolTest {
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
        let count = self.count.clone();

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
                let pending_close = ctx.input(|x| x.viewport().close_requested());

                egui::Window::new("Window in Viewport!").show(ctx, |ui| {
                    ui.label("blah blah");
                    ui.label(format!("Now: {tick}"));

                    if pending_close {
                        count.fetch_add(1, Relaxed);
                        ctx.send_viewport_cmd(egui::ViewportCommand::CancelClose);
                    }

                    if count.load(Relaxed) > 0 {
                        ui.heading("HAHA YOU CANNOT CLOSE ME!");
                    }
                });
            },
        );
    }
}
