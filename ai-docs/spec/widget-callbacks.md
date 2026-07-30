---
title: Widget Callbacks
summary: Registering persistent draw callbacks on the bridge, and the retain/dispose contract plus decorators that control how long they live.
---

# Widget Callbacks

Besides drawing inline from `process()`, callers can register draw callbacks that the bridge
invokes every frame on its own. This lets a system hand the bridge a piece of UI and forget about
it — the callback keeps drawing until it says it is done, or until a sentinel it was bound to
goes away.

## Registering Callbacks {#260730-render-callback-registration}

Two registration points exist, differing only in when in the frame they run:

- `register_render_callback_first(priority, draw)` — runs at frame start, before caller code that
  draws inline in `process()`.
- `register_render_callback_last(priority, draw)` — runs at frame end, after inline drawing.

Frame-start callbacks are the natural place for panels that must claim screen space before other
UI lays itself out; frame-end callbacks are for overlays drawn on top of whatever else happened.

Within each of the two groups, callbacks run in ascending `priority` order — a lower number runs
earlier. Ties keep their existing relative order. Priorities are independent between the two
groups; a frame-start callback always runs before every frame-end callback regardless of number.

A callback receives `&egui::Context` and returns anything convertible into a retain decision.

> [!note] Implementation Gap · 2026-07-30
> Missing behavior. Ties do not keep their existing relative order. Registration inserts at an
> index found by binary search, which is unspecified among equal priorities, so a new callback
> can land ahead of an already-registered peer; and the merge performed after each invocation
> systematically places callbacks registered during that pass ahead of retained ones at the same
> priority. Callers that need a defined order between two callbacks must give them different
> priorities. Tracked by `260730-bug-callback-merge-discards-registrations`.

## Retain and Dispose {#260730-widget-retain-lifetime}

A callback's return value decides whether it stays registered:

- `true` (`Retain`) — keep the callback.
- `false` (`Dispose`) — unregister it; it will not run again.
- `()` (`Unspecified`) — keep the callback. For widget callbacks, an unspecified result is
  retained indefinitely.

The last case is what makes `|ctx| { ... }` with no trailing expression a valid, permanently
registered callback.

Retain decisions compose: combining two decisions yields `Dispose` if either disposes, otherwise
`Retain` if either retains, otherwise `Unspecified`. Decorators use this to layer their own
lifetime rules on top of the caller's return value.

Note that `Unspecified` means the opposite thing for viewports, where it defers to egui and lets
the viewport be disposed when it stops producing output. Only widget callbacks read it as
"retain".

## Registering From Inside a Callback {#260730-reentrant-registration}

A running callback may register further callbacks. The bridge detaches the callback list before
invoking it, so registration from inside is safe and does not deadlock or skip entries.

Callbacks registered this way take effect on the next frame, not the current one. The merged list
is re-sorted by priority afterwards, so a late registration still lands in its correct ordering
position rather than at the end.

> [!note] Implementation Gap · 2026-07-30
> Missing behavior. A registration made from inside a callback is silently discarded whenever
> every callback in that group disposed itself during the same pass: the merge that restores the
> list only combines the two halves when both are non-empty, and otherwise overwrites with the
> survivors. The clearest case is a callback wrapped in `once()` that registers its own
> successor — the successor never runs and nothing is reported. Tracked by
> `260730-bug-callback-merge-discards-registrations`.

## Registration Wakes the UI {#260730-registration-wakes-repaint}

Registering a callback requests a repaint. A newly registered callback therefore appears on the
next frame even if the UI was otherwise idle and nothing else would have triggered a redraw. A
caller does not need to also poke the context to make its widget show up.

## Lifetime Decorators {#260730-draw-callback-decorators}

Any draw callback can be wrapped with decorators before registration. Each returns another draw
callback, so they compose:

- `bind(sentinel)` — disposes the callback once the sentinel reports expired. This is the usual
  way to tie a widget's lifetime to the object that owns it.
- `once()` — runs the callback exactly once, then disposes it regardless of what it returned.
- `expires_at(instant)` — disposes the callback at the given instant.
- `lifespan(duration)` — disposes the callback after the given duration, measured from the moment
  the decorator was applied.

`expires_at` and `lifespan` measure wall-clock time, not game time. A paused or slowed game does
not extend the widget's life, and a widget with a lifespan continues aging while the game is
stopped.

A decorator's disposal wins over the callback's own return value: once the expiry condition
holds, the inner callback is not called again even if it would have returned retain.

## Expiry Sentinels {#260730-expiry-sentinels}

`bind` accepts any expiry sentinel. The supported ones are:

| Sentinel | Expired when |
|---|---|
| `std::rc::Weak<T>` | the strong count reaches zero |
| `std::sync::Weak<T>` | the strong count reaches zero |
| `Arc<AtomicBool>` | the flag is set to `false` |
| `Rc<Cell<bool>>` | the flag is set to `false` |
| `bool` | the value is `false` |

For the flag-shaped sentinels the convention is "true means alive": the widget lives while the
flag holds `true` and is disposed once it is cleared. The weak-pointer sentinels let a widget die
naturally with the object that spawned it, which is the common case in editor plugins.

> [!note] Implementation Gap · 2026-07-30
> Missing behavior. `Gd<T>` is also accepted as a sentinel, but its expiry check has inverted
> polarity: it reports expired while the Godot object is still valid, and reports alive after the
> object is freed. Binding a widget to a `Gd<T>` therefore disposes it on its first frame and, if
> it somehow survives, keeps it registered past the object's destruction. The intended contract is
> the opposite — expired once the instance is no longer valid.

> [!note] Implementation Gap · 2026-07-30
> Unexposed capability. The crate exports an empty `widgets` module. No panel-group or
> widget-spawning API exists behind it; the module is a placeholder with no content, and there is
> no decision yet on whether to build it out or remove it from the public surface.
