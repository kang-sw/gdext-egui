use std::{
    mem::take,
    sync::{atomic::Ordering::Relaxed, mpsc},
    time::{Duration, Instant},
};

use egui::{CursorIcon, ViewportId};
use godot::{
    classes::{self, control::MouseFilter, window, DisplayServer},
    prelude::*,
};
use tap::prelude::Pipe;
use with_drop::with_drop;

use crate::{default, helpers::ToCounterpart, surface};

use super::{
    DeferredCommand, EguiBridge, SurfaceContext, ViewportContext, VIEWPORT_CLOSE_CLOSE,
    VIEWPORT_CLOSE_NONE, VIEWPORT_CLOSE_PENDING, VIEWPORT_CLOSE_REQUESTED,
};

/// Viewport lifecycle private methods.
impl EguiBridge {
    pub(super) fn free_surface(x: Option<SurfaceContext>) {
        if let Some(mut x) = x {
            x.painter.queue_free();

            if let Some(mut x) = x.window {
                x.queue_free();
            }
        }
    }

    pub(super) fn viewport_validate(
        &self,
        id: ViewportId,
        build_with_parent: Option<(ViewportId, egui::ViewportBuilder)>,
    ) {
        // Checkout painter
        let mut surface = with_drop(self.surfaces.borrow_mut().remove(&id), Self::free_surface);

        // Spawn context if viewport id not exist
        let mut should_rebuild = false;
        let mut viewport_lock = self.share.viewports.lock();
        let viewport = match viewport_lock.entry(id) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                if let Some((parent, build)) = build_with_parent {
                    let entry = entry.get_mut();
                    let (patch, recreate) = entry.builder.patch(build);

                    // We don't need to trigger recreation from this flag ... Everything
                    // is configurable through commands.
                    let _ = recreate;

                    if entry.info.parent.is_some_and(|p| p != parent) {
                        // Parent is changed, so we need to recreate this viewport.
                        should_rebuild = true;

                        // In this case, previous updates will be discarded.
                        entry.updates.splice(.., patch);
                    } else {
                        entry.updates.extend(patch);
                    }
                }

                entry.into_mut()
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                should_rebuild = true;

                // Just throw away this ... it'll be replaced by new one.
                let (_tx_update, rx_update) = mpsc::channel();
                let mut init = egui::ViewportBuilder::default();

                // Derive some defaults from parent window
                let gd_wnd_parent = build_with_parent
                    .as_ref()
                    .map(|x| x.0)
                    .and_then(|id| {
                        self.surfaces
                            .borrow_mut()
                            .get(&id)
                            .and_then(|x| x.window.clone())
                    })
                    .unwrap_or_else(|| self.base().get_window().expect("not added in tree!"));

                let (updates, _) = build_with_parent
                    .map(|x| {
                        use tap::prelude::Tap;
                        init.patch(x.1.tap_mut(|init| {
                            if init.position.is_none() {
                                let pos = gd_wnd_parent.get_position().to_alternative();
                                init.position = Some(pos + egui::vec2(25., 25.));
                            }

                            if init.inner_size.is_none() {
                                init.inner_size = Some(egui::vec2(272., 480.));
                            }
                        }))
                    })
                    .unwrap_or_default();

                entry.insert(ViewportContext {
                    repaint_at: Some(Instant::now()),
                    rx_update,
                    close_request: Default::default(),
                    builder: init,
                    target_ui_scale: 1.,
                    updates,
                    paint_this_frame: None,
                    info: default(),
                })
            }
        };

        if surface.is_none() || should_rebuild {
            drop(surface.take());

            // Create channel between new viewport and painter.
            let (tx_viewport, rx_viewport) = mpsc::channel();
            viewport.rx_update = rx_viewport;

            // Rebuild UI.
            let mut gd_painter = surface::EguiViewportBridge::new_alloc();

            let ctx = self.share.egui.clone();
            gd_painter.bind_mut().initiate(
                ctx.clone(),
                id,
                Box::new(move |ev| {
                    // NOTE: cloning egui context into this closure doesn't make cyclic reference
                    // - Both are field of `Self`, which does not refer to each other.

                    tx_viewport.send(ev).ok(); // Failing this is just okay.
                }),
            );

            let tx = self.tx_bg_task.borrow().clone().unwrap();
            gd_painter.connect(
                "resized",
                &Callable::from_fn("Resize", move |_| {
                    // Send repaint request to background worker. Here we don't directly
                    // call `Context::request_repaint` method on context object to prevent
                    // deadlock, as we're not sure when this bound method is called. (it
                    // actually deadlocks on widget initialization)
                    tx.send(DeferredCommand::RequestRepaint(id)).ok();
                    Variant::nil()
                }),
            );

            let gd_wnd = if id == ViewportId::ROOT {
                // Attach directly to this component.
                self.to_gd().add_child(&gd_painter);
                gd_painter.set_owner(&self.to_gd());

                // NOTE: For root viewport...
                //
                // TODO: Merge `IGNORE` behavior between non-root and root.
                // - This is required to implement `add node as child of viewport`
                //   feature.
                //
                // Godot's default `gui_input` handling method, does not propagate inputs
                // into its siblings if they are obscured by this node. Since we're
                // creating a control which covers entire drawable space, and intercepting
                // all inputs, if mouse filter is applied anything other than `IGNORE`
                // would effectively prevent all other non-parent node to receive any
                // input.
                //
                // Therefore, we rather intercept any inputs in `_input()` method, and if
                // we need to consume the input inside egui, we rather make call to
                // `Viewport::set_input_as_handled()` which consumes input even before
                // reaching out to `gui_input()` callbacks of any.
                gd_painter.set_mouse_filter(MouseFilter::IGNORE);

                // To do the tricks
                gd_painter.set_process_input(true);

                None
            } else {
                let builder = &viewport.builder;

                // NOTE: For other viewports, they exclusively use the window, therefore
                // don't need an `input` trick to work correctly.
                gd_painter.set_mouse_filter(MouseFilter::PASS);
                gd_painter.set_process_input(false);

                // Spawn additional window to hold painter.
                let mut gd_wnd = classes::Window::new_alloc();

                self.to_gd().add_child(&gd_wnd);
                gd_wnd.set_owner(&self.to_gd());

                gd_wnd.add_child(&gd_painter);
                gd_painter.set_owner(&gd_wnd);

                // Bind window close request.
                let close_req = viewport.close_request.clone();
                gd_wnd.connect(
                    "close_requested",
                    &Callable::from_fn("SubscribeClose", move |_| {
                        close_req.store(VIEWPORT_CLOSE_REQUESTED, Relaxed);
                        Variant::nil()
                    }),
                );

                // NOTE: List of recreation-only flags
                // - active
                // - app_id
                // - close_button
                // - minimwze_button
                // - maximize_button
                // - title_shown
                // - titlebar_buttons_shown
                // - titlebar_shown
                // - fullsize_content_view
                // - drag_and_drop

                use classes::window::Flags;

                if builder.active.is_some_and(|x| x) {
                    gd_wnd.grab_focus();
                }

                if builder.titlebar_shown.is_some_and(|x| !x) {
                    gd_wnd.set_flag(Flags::BORDERLESS, true);
                }

                Some(gd_wnd)
            };

            *surface = Some(SurfaceContext {
                painter: gd_painter,
                window: gd_wnd,
            });
        }

        let Some(surface) = surface.into_inner() else {
            unreachable!()
        };

        self.apply_viewport_commands(viewport, &surface, id);

        if viewport.close_request.load(Relaxed) == VIEWPORT_CLOSE_PENDING {
            // Close request is accepted, so we should dispose this viewport.
            viewport.close_request.store(VIEWPORT_CLOSE_CLOSE, Relaxed);
        }

        // Update viewport input from surface output.
        self.sync_viewport_info(viewport, &surface, id);

        // Just validate viewport information on input
        let input = viewport.info.clone();

        // After copying required information, drop the lock.
        drop(viewport_lock);

        // Reset viewport info.
        self.share
            .raw_input_template
            .lock()
            .viewports
            .insert(id, input);

        // Checkin surface again.
        self.surfaces.borrow_mut().pipe(|mut x| {
            x.entry(id).or_insert(surface);
        });
    }

    fn apply_viewport_commands(
        &self,
        viewport: &mut ViewportContext,
        surface: &SurfaceContext,
        id: ViewportId,
    ) {
        for command in viewport.updates.drain(..) {
            use egui::ViewportCommand::*;

            let Some(mut window): Option<Gd<classes::Window>> = surface.window.clone() else {
                // Root viewport won't receive any viewport commands.
                continue;
            };

            match command {
                Close => {
                    if id == ViewportId::ROOT {
                        // Ignore close signal to root ... It's simply not allowed!
                        godot_warn!("Root viewport received close request!");
                    } else {
                        // In any other cases; close signal is ignored. User can easily
                        // dispose the viewport by not calling `show_viewport_deferred`
                    }

                    viewport.close_request.store(VIEWPORT_CLOSE_CLOSE, Relaxed);
                }
                CancelClose => {
                    viewport.close_request.store(VIEWPORT_CLOSE_NONE, Relaxed);
                }
                Title(new_title) => {
                    window.set_title(&new_title);
                }
                Transparent(transparent) => {
                    window.set_transparent_background(transparent);
                }
                Visible(visible) => {
                    window.set_visible(visible);
                }
                StartDrag => {
                    // TODO: Implement this
                    //
                    // Set viewport.dragging = true; then until it finishes dragging, get
                    // mouse delta then move the window.
                }
                OuterPosition(pos) => window.set_position(pos.to_alternative()),

                // FIXME: Change painter size; not the containing window size.
                InnerSize(size) => window.set_size(size.to_alternative()),
                MinInnerSize(size) => window.set_min_size(size.to_alternative()),
                MaxInnerSize(size) => window.set_max_size(size.to_alternative()),
                ResizeIncrements(Some(incr)) => {
                    let size = window.get_size();
                    let new_size = size + incr.to_alternative();
                    window.set_size(new_size);
                }
                ResizeIncrements(None) => {}
                BeginResize(_) => {
                    // TODO: Implement this
                }
                Resizable(value) => window.set_flag(window::Flags::RESIZE_DISABLED, !value),
                EnableButtons { .. } => {}
                Minimized(true) => window.set_mode(window::Mode::MINIMIZED),
                Minimized(_) => {}
                Maximized(true) => window.set_mode(window::Mode::MAXIMIZED),
                Maximized(_) => {}
                Fullscreen(true) => window.set_mode(window::Mode::FULLSCREEN),
                Fullscreen(_) => {}
                Decorations(deco) => window.set_flag(window::Flags::BORDERLESS, !deco),
                WindowLevel(level) => {
                    let enabled = match level {
                        egui::WindowLevel::AlwaysOnBottom | egui::WindowLevel::Normal => false,
                        egui::WindowLevel::AlwaysOnTop => true,
                    };

                    window.set_flag(window::Flags::ALWAYS_ON_TOP, enabled);
                }
                Icon(_) => {
                    // TODO: Find way to handle this.
                }
                IMERect(rect) => {
                    window.set_ime_position(rect.to_alternative().position);
                }
                IMEAllowed(allowed) => {
                    window.set_ime_active(allowed);
                }
                IMEPurpose(_why) => {
                    // TODO: How?
                }
                Focus => {
                    window.grab_focus();
                }
                RequestUserAttention(_) => {
                    // No way?
                }
                SetTheme(_) => {
                    // How?
                }
                ContentProtected(_) => {}
                CursorPosition(_pos) => {}
                CursorGrab(_) => {}
                CursorVisible(_) => {
                    // TODO: How can we achieve this in safe manner?
                    // - e.g. If user simply disposed EGUI after hiding cursor...
                }
                MousePassthrough(enabled) => {
                    window.set_flag(window::Flags::MOUSE_PASSTHROUGH, enabled);
                }
                Screenshot(_) => {
                    // TODO: How?
                }
                RequestCut => {
                    // TODO
                }
                RequestCopy => {
                    // TODO
                }
                RequestPaste => {
                    // TODO
                }
            }
        }
    }

    fn sync_viewport_info(
        &self,
        viewport: &mut ViewportContext,
        surface: &SurfaceContext,
        _id: ViewportId,
    ) {
        'wnd: {
            let gd_wnd = match surface.window.clone() {
                Some(wnd) => wnd,
                None => {
                    if let Some(wnd) = surface.painter.get_window() {
                        wnd
                    } else {
                        break 'wnd;
                    }
                }
            };

            let info = &mut viewport.info;

            let inner_pos = gd_wnd.get_position().cast_float() + surface.painter.get_position();
            let inner_size = surface.painter.get_size();

            let gd_ds = DisplayServer::singleton();
            let id_screen = gd_ds
                .window_get_current_screen_ex()
                .window_id(gd_wnd.get_window_id())
                .done();
            let scale = gd_ds.screen_get_scale_ex().screen(id_screen).done();

            info.inner_rect = Some(Rect2::new(inner_pos, inner_size).to_counterpart());
            info.focused = Some(gd_wnd.has_focus());
            info.native_pixels_per_point = Some(scale);
            info.fullscreen = Some(gd_wnd.get_mode() == window::Mode::FULLSCREEN);
            info.minimized = Some(gd_wnd.get_mode() == window::Mode::MINIMIZED);
            info.maximized = Some(gd_wnd.get_mode() == window::Mode::MAXIMIZED);
            info.monitor_size = Some(
                gd_ds
                    .screen_get_size_ex()
                    .screen(id_screen)
                    .done()
                    .to_counterpart(),
            );
            info.outer_rect = Some(egui::Rect::from_min_size(
                gd_wnd.get_position().to_alternative(),
                gd_wnd.get_size().to_counterpart(),
            ));
        }
    }

    pub(super) fn viewport_start_frame(&self, id: ViewportId) {
        // NOTE: Seems recursive call to begin_frame is handled by stack internally.
        let mut raw_input = self.share.raw_input_template.lock().clone();

        {
            let mut viewport = self.share.viewports.lock();
            let viewport = viewport.get_mut(&id).unwrap();

            raw_input.events.extend(viewport.rx_update.try_iter());
            raw_input.screen_rect = viewport.info.inner_rect.map(|x| {
                egui::Rect::from_min_size(egui::Pos2::ZERO, x.size() / viewport.target_ui_scale)
            });

            raw_input.focused = viewport.info.focused.unwrap_or_default();
            raw_input.viewport_id = id;

            // Just set repaint schedule to far future.
            viewport.repaint_at = Some(Instant::now() + Duration::from_secs(3600));

            // If close request is delivered from platform, forward the event to EGUI that
            // allow user logic to handle this. (e.g. cancel the close request)
            if viewport.close_request.load(Relaxed) == VIEWPORT_CLOSE_REQUESTED {
                viewport
                    .close_request
                    .store(VIEWPORT_CLOSE_PENDING, Relaxed);
                raw_input
                    .viewports
                    .get_mut(&id)
                    .unwrap()
                    .events
                    .push(egui::ViewportEvent::Close);
            }
        }

        self.share.egui.begin_pass(raw_input);
    }

    pub(super) fn viewport_end_frame(&self, id: ViewportId) {
        // Retrieve viewport-wise output.
        let mut output = self.share.egui.end_pass();

        let paints = take(&mut output.shapes);
        let ppi = output.pixels_per_point;

        let primitives = self.share.egui.tessellate(paints, ppi);
        self.share
            .viewports
            .lock()
            .get_mut(&id)
            .unwrap()
            .pipe(|vp| {
                vp.paint_this_frame = Some(primitives);
                vp.target_ui_scale = ppi;
            });

        let mut gd_wnd = self
            .surfaces
            .borrow_mut()
            .get(&id)
            .and_then(|x| x.painter.get_window())
            .expect("A painter should be spawned under any valid window!");

        if let Some(ime) = output.platform_output.ime.take() {
            // XXX: Is calling this every frame safe?
            gd_wnd.set_ime_active(true);
            gd_wnd.set_ime_position(ime.cursor_rect.min.to_alternative());
        } else {
            gd_wnd.set_ime_active(false);
        }

        // Handle platform outputs accumulated from all viewports.
        {
            let egui::PlatformOutput {
                commands,
                events,
                mutable_text_under_cursor,

                // Handled by each viewport.
                cursor_icon,
                ..
            } = take(&mut output.platform_output);

            let mut ds = DisplayServer::singleton();

            for cmd in commands {
                match cmd {
                    egui::OutputCommand::CopyText(copied_text) => {
                        ds.clipboard_set(&copied_text);
                    }
                    egui::OutputCommand::CopyImage(_color_image) => {
                        godot_warn!("gdext_egui doesn't support image clipboard copying")
                    }
                    egui::OutputCommand::OpenUrl(open_url) => {
                        open::that(open_url.url).ok();
                    }
                }
            }

            if mutable_text_under_cursor {
                // XXX: Do we need virtual board ...?
            }

            for _event in events {
                // We're not interested in widget outputs
            }

            let overwrite_cursor = if self.cursor_shape.borrow().is_some() {
                // Do not overwrite meaningful cursor with `None` or `Default`
                !matches!(cursor_icon, CursorIcon::None | CursorIcon::Default)
            } else {
                // Prevent `None` cursor disturbing the engine's cursor control
                cursor_icon != CursorIcon::None
            };

            if overwrite_cursor {
                *self.cursor_shape.borrow_mut() = Some(cursor_icon);
            }
        }

        // Accumulate outputs to primary output.
        self.share.full_output.lock().append(output);

        // Call setup scripts that was queued during frame.
        for script in self.setup_scripts.borrow_mut().drain(..) {
            script(&self.share.egui);
        }
    }
}
