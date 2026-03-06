use std::{mem::take, sync::Arc};

use egui::ViewportId;
use godot::{
    classes::{self, control::LayoutPreset, Control},
    prelude::*,
};
use tap::prelude::{Pipe, Tap};

use crate::helpers::try_upgrade_gd;

use super::{DeferredCommand, EguiBridge, WidgetRetain};

/// Frame lifecycle private methods.
impl EguiBridge {
    pub(super) fn try_initiate(&self) {
        if self.tx_bg_task.borrow().is_some() {
            // It's already initiated.
            return;
        }

        // Spawn background worker thread
        {
            let (tx_b, rx_b) = std::sync::mpsc::channel::<DeferredCommand>();

            assert!(self.tx_bg_task.replace(Some(tx_b)).is_none());
            assert!(self.rx_bg_task.replace(Some(rx_b)).is_none());
        };

        // Setup egui context & repaint callback.
        (&self.share.egui).pipe(|ctx| {
            let w_share = Arc::downgrade(&self.share);

            ctx.set_embed_viewports(false);
            ctx.set_request_repaint_callback({
                // Prevent cyclic reference; `share` is already holding the reference!
                let w_share = w_share.clone();
                move |repaint| {
                    let Some(share) = w_share.upgrade() else {
                        godot_print!("Repaint requested for disposed egui bridge: {repaint:?}");
                        return;
                    };

                    share.repaint(repaint);
                }
            });
        });
    }

    pub(super) fn handle_bg_message(&self) {
        let Some(rx_b) = self.rx_bg_task.borrow_mut().take() else {
            return;
        };

        let ctx = self.share.egui.clone();

        for msg in rx_b.try_iter() {
            match msg {
                DeferredCommand::RequestRepaint(viewport_id) => {
                    ctx.request_repaint_of(viewport_id);
                }
            }
        }

        self.rx_bg_task.replace(Some(rx_b));
    }

    pub(super) fn try_dispose(&mut self) {
        // Join background worker thread. To do this, channel should be closed first.
        let Some(_bg) = self.tx_bg_task.take() else {
            // Service is just not initialized.
            return;
        };

        self.rx_bg_task.take();
    }

    pub(super) fn try_start_frame(&self) {
        assert!(std::thread::current().id() == self.share.main_thread_id);

        // Only perform frame start when necessary.
        if !self.share.try_advance_frame() {
            return;
        }

        // Just lazily initiate the system.
        self.try_initiate();

        // Register immediate renderer for this frame.
        // NOTE: Capture only the InstanceId (a Copy integer) instead of a Variant/WeakRef,
        // because this closure is stored in egui's thread-local and may outlive the Godot
        // engine binding — dropping a Variant after engine shutdown causes a panic.
        let self_id = self.to_gd().instance_id();

        egui::Context::set_immediate_viewport_renderer(move |ctx, mut viewport| {
            let Ok(this) = Gd::<Self>::try_from_instance_id(self_id) else {
                // Object has been freed.
                return;
            };

            let this = this.bind();

            let p_src = this.share.egui.input(|x| x as *const _);
            let p_new = ctx.input(|x| x as *const _);

            if p_src != p_new {
                // Another EGUI runtime?
                return;
            }

            this.viewport_validate(
                viewport.ids.this,
                Some((viewport.ids.parent, viewport.builder)),
            );
            this.viewport_start_frame(viewport.ids.this);

            (viewport.viewport_ui_cb)(ctx);
            this.viewport_end_frame(viewport.ids.this);
        });

        // Gather global input information
        let share = self.share.clone();
        share
            .viewports
            .lock()
            .pipe(|vp| {
                vp.iter()
                    .map(|(id, value)| (*id, value.info.clone()))
                    .collect::<egui::ViewportIdMap<_>>()
            })
            .pipe(|vp| {
                let mut inp = share.raw_input_template.lock();
                inp.viewports = vp;
                inp.time = Some(classes::Time::singleton().get_ticks_usec() as f64 / 1e6);

                // XXX: 256~ 65536 texture size limitation => is this practical?
                inp.max_texture_side = Some(1 << (self.max_texture_bits as usize).clamp(8, 16));
                inp.modifiers = {
                    use godot::global::Key as GdKey;

                    let gd_input = classes::Input::singleton();
                    let is_pressed = |k: GdKey| gd_input.is_key_pressed(k);

                    egui::Modifiers {
                        alt: is_pressed(GdKey::ALT),
                        ctrl: is_pressed(GdKey::CTRL),
                        shift: is_pressed(GdKey::SHIFT),
                        command: is_pressed(GdKey::CTRL),
                        mac_cmd: is_pressed(GdKey::META),
                    }
                };
            });

        // Before starting a frame, check if we can spawn separate windows for viewport.
        self.share.egui.set_embed_viewports(
            self.base()
                .get_viewport()
                .unwrap()
                .is_embedding_subwindows(),
        );

        // Start root frame as normal.
        self.viewport_validate(egui::ViewportId::ROOT, None);

        // After root region is initialized, try sync it with root region.
        'sync: {
            let Some(w_target) = self.root_region_sync.take() else {
                break 'sync;
            };

            let Some(target) = try_upgrade_gd::<Control>(w_target.clone()) else {
                self.reset_root_region_sync();
                break 'sync;
            };

            // Target is still valid; return it back to the list.
            self.root_region_sync.set(Some(w_target));

            // Check if size mismatches
            let mut surfaces = self.surfaces.borrow_mut();
            let root = surfaces.get_mut(&ViewportId::ROOT).unwrap();

            let target_rect = target.get_global_rect();
            let root_rect = root.painter.get_global_rect();

            let err_pos = target_rect.position - root_rect.position;
            let err_size = target_rect.size - root_rect.size;

            // We use error approximation here as the sync size is result of calculation
            // => which may have floating point errors.
            if err_pos.length_squared() < 1e-4 && err_size.length_squared() < 1e-4 {
                // No need to sync
                break 'sync;
            }

            // Sync root region
            root.painter.set_global_position(target_rect.position);
            root.painter.set_size(target_rect.size);
        }

        self.viewport_start_frame(egui::ViewportId::ROOT);

        // Call registered callbacks for start of the frames.
        self.invoke_registered_callbacks(true);
    }

    pub(super) fn invoke_registered_callbacks(&self, first: bool) {
        let get_cb = || {
            if first {
                self.widget_callbacks_first.borrow_mut()
            } else {
                self.widget_callbacks_last.borrow_mut()
            }
        };

        let mut callbacks = { take(&mut *get_cb()) };

        // We release borrow here to make callbacks safely invoke
        // `register_render_callback_*` methods.

        callbacks.retain_mut(|(_, cb)| !cb(&self.share.egui).disposed());

        let mut cbs = get_cb();
        let should_sort = !cbs.is_empty() && !callbacks.is_empty();

        if should_sort {
            cbs.extend(callbacks);
            cbs.sort_by_key(|(p, ..)| *p);
        } else {
            *cbs = callbacks;
        }
    }

    pub(super) fn reset_root_region_sync(&self) {
        self.surfaces
            .borrow_mut()
            .get_mut(&egui::ViewportId::ROOT)
            .unwrap()
            .pipe(|x| {
                x.painter
                    .set_anchors_and_offsets_preset(LayoutPreset::FULL_RECT)
            });

        // just to ensure.
        self.root_region_sync.set(None);
    }

    pub(super) fn finish_frame(&mut self) {
        use std::collections::{hash_map, HashSet, VecDeque};

        let share = self.share.clone();

        /* ------------------------- Spawned Widget / Viewport Handling ------------------------- */
        // Deal with registered callbacks for frame end.
        self.invoke_registered_callbacks(false);

        // Deal with spawned viewports.
        let viewports = take(&mut *share.spawned_viewports.lock()).tap_mut(|viewports| {
            // Check if any of the spawned viewports should be disposed.
            viewports.retain(|id, value| {
                if *value.dispose.lock() == WidgetRetain::Dispose {
                    false
                } else {
                    let ui_cb = value.repaint.clone();
                    share
                        .egui
                        .show_viewport_deferred(*id, value.builder.clone(), move |ctx, _| {
                            ui_cb(ctx);
                        });

                    true
                }
            });
        });

        viewports.pipe(|mut viewports| {
            // Check-in viewports list.
            let mut lock = share.spawned_viewports.lock();

            // Overwrite previous viewports with newly spawned ones, if exist.
            viewports.extend(lock.drain());
            *lock = viewports;
        });

        /* ------------------------------ Viewport Deltas Handling ------------------------------ */

        // End main frame loop.
        self.viewport_end_frame(egui::ViewportId::ROOT);

        // Handle viewport changes from output, visit each viewports
        let mut remaining_viewports = share
            .viewports
            .lock()
            .keys()
            .copied()
            .collect::<HashSet<_>>();

        let mut viewports = VecDeque::new();
        let now = std::time::Instant::now();

        loop {
            // Not any lock should be held here.
            viewports.extend(take(&mut share.full_output.lock().viewport_output));

            let Some((vp_id, vp_out)) = viewports.pop_front() else {
                break;
            };

            let scheduled = if let Some(viewport) = share.viewports.lock().get_mut(&vp_id) {
                if viewport
                    .close_request
                    .load(std::sync::atomic::Ordering::Relaxed)
                    == super::VIEWPORT_CLOSE_CLOSE
                {
                    // If this is `PENDING`, it means the user side renderer has already
                    // seen viewport close request, however, didn't deal with it, which
                    // means accepted disposal of viewport close.

                    // Simply by not invoking subsequent rendering logic, (more precisely,
                    // not removing viewport ID from `remaining_viewports`), we can safely
                    // dispose this viewport.
                    continue;
                }

                // Commands are only meaningful when viewport already present.
                viewport.updates.extend(vp_out.commands);

                if viewport.repaint_at.is_some_and(|x| x < now) {
                    viewport.repaint_at = None; // Clear repaint timer until next request
                    true
                } else {
                    // If viewport is being closed now, force repainting it.
                    viewport
                        .close_request
                        .load(std::sync::atomic::Ordering::Relaxed)
                        == super::VIEWPORT_CLOSE_REQUESTED
                }
            } else {
                false
            };

            // Don't need to check if remove succeeded; as it can be a viewport created
            // inside rendering loop; which is perfectly valid egui API call.
            let _ = remaining_viewports.remove(&vp_id);

            // Validate viewport.
            self.viewport_validate(vp_id, Some((vp_out.parent, vp_out.builder)));

            if let Some(ui_cb) = vp_out.viewport_ui_cb.filter(|_| scheduled) {
                // Check if we should repaint this deferred viewport. For root and
                // immediate viewports, these methods are already invoked!

                self.viewport_start_frame(vp_id);
                // Populate renderings
                ui_cb(&self.share.egui);
                self.viewport_end_frame(vp_id);
            }
        }

        // Deal with removed viewports
        for id in remaining_viewports {
            match share.spawned_viewports.lock().entry(id) {
                hash_map::Entry::Occupied(entry) => {
                    if *entry.get().dispose.lock() == WidgetRetain::Retain {
                        // The widget didn't agree to close, so we put it back to the list.
                        // Other than `Retain` treated as `Dispose`.
                        continue;
                    }

                    // Spawned viewport also agreed to close.
                    entry.remove();
                }
                hash_map::Entry::Vacant(_) => (),
            }

            // Painter should be freed first, then viewport.
            Self::free_surface(self.surfaces.borrow_mut().remove(&id));

            // Remove viewport from context. Assertion here since we've retrieved
            // remaining_viewports from viewport list itself, any 'subtractive'
            // modification on viewports list is internal logic error!
            assert!(share.viewports.lock().remove(&id).is_some());
        }

        /* -------------------------------- Frame Output Handling ------------------------------- */

        // Cleanup full_output for next frame.
        let egui::FullOutput {
            platform_output: _,
            textures_delta:
                egui::TexturesDelta {
                    set: textures_created,
                    free: textures_freed,
                },
            shapes,
            pixels_per_point: _,
            viewport_output: _,
        } = take(&mut *self.share.full_output.lock());

        debug_assert!(shapes.is_empty(), "logic error - shape is viewport-wise");

        // Handle cursor shape
        if let Some(cursor) = self.cursor_shape.take() {
            type CS = classes::display_server::CursorShape;
            let mut ds = classes::DisplayServer::singleton();

            ds.cursor_set_shape(match cursor {
                egui::CursorIcon::Default => CS::ARROW,
                egui::CursorIcon::Help => CS::HELP,
                egui::CursorIcon::PointingHand => CS::POINTING_HAND,
                egui::CursorIcon::Wait => CS::WAIT,
                egui::CursorIcon::Crosshair => CS::CROSS,
                egui::CursorIcon::Text => CS::IBEAM,
                egui::CursorIcon::VerticalText => CS::IBEAM,
                egui::CursorIcon::NotAllowed => CS::FORBIDDEN,
                egui::CursorIcon::AllScroll => CS::MOVE,
                egui::CursorIcon::ResizeHorizontal => CS::HSIZE,
                egui::CursorIcon::ResizeNeSw => CS::BDIAGSIZE,
                egui::CursorIcon::ResizeNwSe => CS::FDIAGSIZE,
                egui::CursorIcon::ResizeVertical => CS::VSIZE,
                egui::CursorIcon::ResizeEast => CS::HSIZE,
                egui::CursorIcon::ResizeSouthEast => CS::FDIAGSIZE,
                egui::CursorIcon::ResizeSouth => CS::VSIZE,
                egui::CursorIcon::ResizeSouthWest => CS::BDIAGSIZE,
                egui::CursorIcon::ResizeWest => CS::HSIZE,
                egui::CursorIcon::ResizeNorthWest => CS::FDIAGSIZE,
                egui::CursorIcon::ResizeNorth => CS::VSIZE,
                egui::CursorIcon::ResizeNorthEast => CS::BDIAGSIZE,
                egui::CursorIcon::ResizeColumn => CS::HSIZE,
                egui::CursorIcon::ResizeRow => CS::VSIZE,
                _cursor => CS::ARROW,
            });
        }

        /* -------------------------------------- Painting -------------------------------------- */

        // Handle new textures from output.
        for (id, delta) in textures_created {
            self.textures.update_texture(id, delta);
        }

        // Paint all viewports
        for (id, mut paint) in self.surfaces.borrow_mut().clone() {
            let Some((primitives, ui_scale)) = self
                .share
                .viewports
                .lock()
                .get_mut(&id)
                .unwrap()
                .pipe(|vp| vp.paint_this_frame.take().map(|x| (x, vp.target_ui_scale)))
            else {
                // This viewport is not re-rendered this frame.
                continue;
            };

            paint
                .painter
                .bind_mut()
                .draw(&self.textures, primitives, ui_scale);
        }

        // Handle disposed textures from output.
        for id in textures_freed {
            self.textures.free_texture(id);
        }

        /* ---------------------------------------- Done. --------------------------------------- */

        // Finish this frame.
        self.share.finish_frame();
    }

    /// Start frame in thread-safe manner.
    pub(super) fn queue_try_start_frame(&self) {
        if std::thread::current().id() == self.share.main_thread_id {
            self.try_start_frame();
            return;
        }

        if !self.share.try_advance_frame() {
            return;
        }

        self.to_gd()
            .call_deferred(symbol_string!(Self, __internal_try_start_frame_inner), &[]);
    }
}
