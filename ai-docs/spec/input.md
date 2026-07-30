---
title: Input
summary: How Godot input events reach egui, when egui consumes them from the rest of the game, and which keys, buttons, modifiers, and clipboard operations are supported.
---

# Input

Each viewport surface translates Godot `InputEvent`s into egui events and decides whether the
event should also continue to the rest of the game. The translation is one-directional: Godot is
the source of truth for input, egui is fed from it.

## Input Routing {#260730-input-routing}

The root viewport and spawned viewports take opposite routes through Godot's input system.

The root surface is a `Control` covering the whole drawable area. If it participated in normal
GUI input it would shadow every sibling control in the scene, since Godot does not propagate GUI
input past an obscuring node. So the root surface sets its mouse filter to ignore, listens on the
global unhandled-input path instead, and marks the input as handled on the viewport when egui
claims it. Marking it handled stops the event before it reaches any control's GUI input, which
achieves the intended "egui is on top" behavior without blocking events egui does not want.

Spawned viewports own their window exclusively, so they need no such trick: they take normal GUI
input and accept the event when egui claims it.

The practical consequence for callers is that egui always sits above the rest of the UI, and
non-egui controls keep receiving everything egui declines.

## Input Consumption {#260730-input-consumption}

Whether an event continues to the game depends on the event kind and on what egui currently
wants:

| Event | Forwarded to egui | Consumed from the game when |
|---|---|---|
| Mouse motion | always | the pointer is over an egui area |
| Mouse button / wheel | always | egui wants pointer input |
| Key | always | egui wants keyboard input |
| Anything else | no | never |

egui therefore sees the full event stream regardless of consumption — hover states stay correct
even for events the game also handles — while the game only loses the events egui is actually
using.

A mouse button that egui consumes also grabs Godot focus for the surface, so a click into an
egui text field takes keyboard focus away from other Godot controls as a user would expect.

Event kinds with no translation — joypad, touch, gestures, actions — are neither forwarded nor
consumed.

## Pointer Input {#260730-pointer-input}

Pointer positions are converted from Godot's global coordinates into surface-local coordinates
and then divided by the viewport's UI scale, so egui receives logical positions that stay correct
under zoom and under a root region synced to a smaller rect.

Mouse buttons map as follows:

| Godot | egui |
|---|---|
| Left | Primary |
| Right | Secondary |
| Middle | Middle |
| Extra button 1 | Extra1 |
| Extra button 2 | Extra2 |

Any other button is dropped and never reaches egui. Canceled mouse button events are ignored
entirely.

## Wheel and Zoom {#260730-wheel-and-zoom}

Godot reports wheel input as four discrete buttons; these become egui wheel events measured in
lines: wheel up/down produce vertical scrolling, wheel left/right produce horizontal scrolling.

The scroll magnitude comes from the event's factor. When the platform reports no factor, a
default of 4 lines is used, so wheel input still scrolls a sensible amount on platforms that omit
it.

Holding Ctrl while scrolling additionally emits a zoom event, with the wheel delta interpreted as
a power of two — one notch up doubles, one notch down halves. The wheel event is still emitted
alongside the zoom event rather than being replaced by it.

## Keyboard and Text {#260730-keyboard-text-input}

Key events produce up to two egui events. A key press whose Unicode value is a printable
character (32 or above) produces a text-input event carrying that character; independently, the
key itself produces a key event when it has a mapping.

Key events carry the pressed state, the modifier state, and a repeat flag taken from Godot's echo
flag, so held-key repetition works in text fields.

Non-printable keys — arrows, function keys, Escape — produce only the key event. Keys with no
mapping produce no key event, but still produce a text event if they carry a printable character,
which is how punctuation and symbols outside the mapping table continue to work for typing.

> [!note] Implementation Gap · 2026-07-30
> Missing behavior. IME composition input is not read: only the IME box position and enable flag
> are driven, from the viewport side. Composing text in Chinese, Japanese, or Korean does not
> reach egui, so egui text fields are effectively ASCII-and-direct-input only.

## Key Mapping Coverage {#260730-key-mapping-coverage}

The mapping covers what a UI needs and deliberately stops short of the full Godot key set:

- Letters A–Z and digits 0–9, plus keypad digits 0–9 (mapped to the same digit keys).
- Function keys F1 through F20.
- Navigation and editing: Escape, Tab, Backspace, Enter (both main and keypad), Insert, Delete,
  Home, End, Page Up, Page Down, arrow keys, Space.
- Punctuation: plus, comma, minus, period, slash, colon, semicolon, backslash, and square
  brackets. Curly braces map onto the square bracket keys.

Everything else — media keys, launcher keys, F21 and above, keypad operators, modifier keys as
standalone keys, and most shifted-symbol keys — has no mapping and produces no key event.

> [!note] Implementation Gap · 2026-07-30
> Missing behavior. The physical-key field on each key event is filled from the same logical
> keycode as the key itself, so egui cannot distinguish logical from physical keys. On non-QWERTY
> layouts, shortcuts that egui resolves by physical position will resolve by logical letter
> instead.

## Modifiers {#260730-modifier-mapping}

Per-event modifiers are taken from the event's own modifier mask: shift, alt, and control, with
egui's platform-independent command modifier mirroring control.

Independently, at the start of each frame the bridge samples the current modifier state directly
from the engine, so modifier state is correct even on frames where no key event arrived — for
example when the user presses Ctrl and then scrolls without releasing.

> [!note] Implementation Gap · 2026-07-30
> Missing behavior. The two modifier paths disagree on macOS. The per-event path derives both
> control and the mac command flag from Godot's combined command-or-control mask, while the
> per-frame sample reads the physical Control and Meta keys separately. On macOS the same physical
> key state can therefore be reported differently depending on which path produced it.

## Clipboard {#260730-clipboard-integration}

Ctrl+C, Ctrl+X, and Ctrl+V are intercepted before normal key translation and turned into egui's
copy, cut, and paste events. Paste reads the system clipboard through Godot's display server, so
egui text fields exchange text with the rest of the desktop.

In the other direction, text copied out of egui is written to the system clipboard.

Copying an image out of egui is not supported: the attempt is reported as a warning in the Godot
output and the clipboard is left unchanged.

## Focus and Pointer Notifications {#260730-focus-pointer-notifications}

Godot's control notifications drive egui's window-level input state:

- Gaining or losing focus tells egui the window's focus state changed, which is what makes text
  cursors blink only in the focused window.
- The pointer leaving the surface tells egui the pointer is gone, clearing hover highlights
  instead of leaving them stuck on the last hovered widget.

Every forwarded input event also requests a repaint of its viewport, so the UI responds
immediately rather than on the next frame that happens to be drawn for another reason.

## Drag and Drop from Godot {#260730-godot-drop-into-egui}

A drag started elsewhere in Godot can be dropped onto an egui surface. The surface accepts any
payload type while egui wants pointer input, and delivers the dropped `Variant` into egui's
drag-and-drop state wrapped so that egui code can retrieve the original Godot value.

Drop position is not consulted; whether the drop is accepted is decided purely by whether egui
considers the pointer to be over its own UI.

> [!note] Implementation Gap · 2026-07-30
> Missing behavior. Dragging out of egui is not implemented — the surface always reports an empty
> drag payload, so neither egui-to-Godot nor egui-to-egui drags start. Only the Godot-to-egui
> direction works.

## Opening Links {#260730-open-url-output}

A hyperlink activated inside egui is opened with the system's default handler for the URL, the
same as clicking a link outside the game. This works for any URL scheme the platform recognizes,
not only http.
