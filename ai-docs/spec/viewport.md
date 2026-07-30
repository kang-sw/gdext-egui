---
title: Viewports
summary: How egui viewports map onto Godot surfaces and native windows, including spawning, window commands, close negotiation, and root region placement.
---

# Viewports

An egui viewport is a drawing surface with its own screen rect and input focus. `gdext-egui`
realizes each viewport as a Godot node: the root viewport is a `Control` filling the bridge's
canvas, and every additional viewport is a real Godot `Window` containing its own `Control`.
Callers reach viewports either through this crate's helpers or through egui's own viewport API on
the context — both land in the same machinery.

## Root Viewport {#260730-root-viewport}

Every bridge always has a root viewport. It is created on the first frame and lives as long as
the bridge is in the tree. Its surface is added as a child of the bridge node and, by default,
stretches to fill the full available rect.

The root viewport has no backing `Window` of its own; it draws into the window the bridge already
lives in. As a consequence it ignores viewport commands entirely — title, size, position,
decorations, close, and every other window command are silently dropped for the root. Callers who
need to control the root's placement use root region sync instead.

## Deferred Viewport Spawning {#260730-viewport-spawn-deferred}

`viewport_spawn(id, builder, show)` registers a viewport that renders at the start of every
subsequent frame by invoking `show`. Registering an id that already exists replaces the previous
registration.

The return value of `show` controls the viewport's lifetime:

- `true` — keep the viewport open.
- `false` — close and free the viewport immediately.
- `()` — leave the decision to egui: the viewport stays open while egui keeps producing output
  for it, and is disposed once it stops, which is what happens when the user closes the window.

Registration is safe to perform from any thread; the frame it needs is scheduled onto the main
thread rather than started in place. Rendering itself always happens on the main thread.

When a viewport is disposed, its `Control` and its `Window` are queued for freeing together, so
closing a viewport removes the whole native window.

## Immediate Viewport Rendering {#260730-viewport-immediate}

`viewport_immediate(id, builder, show)` renders a viewport inside the current frame and returns
whatever `show` returns. It starts a frame first if none is open.

Unlike a spawned viewport it holds no registration: it exists only for frames in which the caller
calls it. This makes it the right choice for a viewport whose existence is already driven by
caller-side state, and the wrong choice for one that must survive frames where the caller does
not run.

Immediate viewports opened through egui's own `Context::show_viewport_immediate` are routed
through the same path. If the context invoking the renderer is not this bridge's context, the
request is ignored rather than drawn into the wrong runtime — two bridges in one scene do not
interfere.

## Native Window Defaults {#260730-native-window-defaults}

A non-root viewport gets a Godot `Window` parented under the bridge. Where the caller's
`ViewportBuilder` leaves a property unset, these defaults apply:

- **Position** — the parent window's position offset by 25 × 25 pixels, so a spawned window is
  visibly offset rather than exactly covering its parent.
- **Inner size** — 272 × 480 pixels.

Two builder properties are applied only at window creation time: `active` grabs focus for the new
window, and `titlebar_shown: false` creates it borderless. Changing them on a later frame has no
effect, because the builder is only consulted when the window is built. Everything else is
adjusted through viewport commands after creation.

If a viewport's parent changes, its window is rebuilt from scratch and any queued commands from
the previous incarnation are discarded.

## Embedded Viewport Mode {#260730-embed-follows-godot-subwindows}

At the start of every frame, the bridge reads whether the Godot viewport it lives in embeds
subwindows, and configures egui to match. In a project configured to embed subwindows, egui
viewports are drawn inside the main window instead of becoming separate OS windows; in a project
that does not, they become real windows.

Callers therefore do not choose between embedded and native viewports — Godot's own window
setting decides, and the behavior follows the rest of the project automatically.

## Viewport Commands {#260730-viewport-commands}

Commands sent through egui (`send_viewport_cmd` and equivalents) are applied to the viewport's
Godot window at the start of the next frame. Supported commands:

| Command | Effect |
|---|---|
| `Title` | Sets the window title |
| `Visible` | Shows or hides the window |
| `Transparent` | Toggles transparent window background |
| `OuterPosition` | Moves the window |
| `InnerSize`, `MinInnerSize`, `MaxInnerSize` | Sets window size and size limits |
| `ResizeIncrements(Some)` | Grows the window by the given increment |
| `Resizable` | Enables or disables user resizing |
| `Decorations` | Toggles the border and titlebar |
| `WindowLevel` | `AlwaysOnTop` pins the window above others; other levels clear the flag |
| `Minimized(true)`, `Maximized(true)`, `Fullscreen(true)` | Switches the window mode |
| `Focus` | Grabs focus |
| `MousePassthrough` | Lets mouse input pass through the window |
| `IMERect`, `IMEAllowed` | Positions and enables the platform IME box |
| `Close`, `CancelClose` | See close negotiation below |

The following commands are accepted and ignored: `StartDrag`, `BeginResize`, `EnableButtons`,
`Icon`, `IMEPurpose`, `RequestUserAttention`, `SetTheme`, `ContentProtected`, `CursorPosition`,
`CursorGrab`, `CursorVisible`, `Screenshot`, `RequestCut`, `RequestCopy`, `RequestPaste`, and
`ResizeIncrements(None)`. Sending them is harmless but produces no observable change.

`Minimized(false)`, `Maximized(false)`, and `Fullscreen(false)` are also ignored, so a window can
be put into a mode by command but not returned to windowed mode by command.

> [!note] Implementation Gap · 2026-07-30
> Missing behavior. `InnerSize` resizes the containing window rather than the drawing surface, so
> the resulting client area is off by the window decorations.

## Close Negotiation {#260730-close-negotiation}

When the user closes a viewport's window through the OS, the close is not immediate. The request
is recorded, and on the next frame egui receives a `Close` viewport event, giving the caller's
draw code a chance to react — typically by checking `close_requested()` and responding with
`CancelClose` to keep the window open. A viewport that does not cancel is disposed at the end of
that frame.

`CancelClose` clears a pending request. A `Close` command sent by the caller marks the viewport
for disposal directly, without a negotiation round.

The root viewport cannot be closed this way: it receives no viewport commands at all, so a
`Close` aimed at the root has no effect.

## Root Region Sync {#260730-root-region-sync}

`sync_root_region(Some(control))` makes the root viewport's surface track the given Godot
`Control`: every frame, if the surface's global rect differs from the target's, the surface is
moved and resized to match. This is what lets an egui UI occupy a specific panel — an editor
plugin's main-screen area, for example — instead of the whole window.

`sync_root_region(None)` unregisters the tracking and restores the surface to filling the full
rect. The same restoration happens automatically if the tracked `Control` is freed, so a
destroyed target does not leave the root stuck at a stale rect.

Comparison uses an epsilon rather than exact equality, because the target rect is computed and
carries floating-point error.

## Viewport Info Reported to egui {#260730-viewport-info-sync}

Each frame, every viewport reports platform state back to egui so that caller draw code can query
it: inner rect, outer rect, focus state, fullscreen/minimized/maximized mode, the size of the
monitor the window is on, and the native pixels-per-point taken from that monitor's display
scale. Multi-monitor setups with differing scale factors are handled per window, since the screen
is resolved from the window's current position.

egui's screen rect for a viewport is the surface size divided by the viewport's UI scale, so
zooming changes the logical size egui lays out against rather than the physical surface.

> [!note] Implementation Gap · 2026-07-30
> Missing behavior. `viewport_spawn_as_child` and `attach_node_to_viewport` are public but do
> nothing: the former has an empty body, and the latter always fails by returning the node back to
> the caller. Their documented intent — hosting a viewport under an arbitrary `Control` parent, and
> placing Godot nodes inside a viewport's window — is not implemented. The blocker recorded in
> source is that root and non-root surfaces use different mouse-filter strategies, which must be
> unified first.
