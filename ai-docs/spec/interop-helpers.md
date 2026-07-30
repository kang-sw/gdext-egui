---
title: Interop Helpers
summary: The egui re-export, Godot-to-egui geometry conversions, weak-reference helpers, and the drag-and-drop payload wrapper exposed at the crate root.
---

# Interop Helpers

Beyond the bridge itself, the crate root exposes the small pieces of glue callers need when
moving values between Godot and egui: the egui crate itself, geometry conversions, weak handle
helpers, and a payload wrapper for drag and drop.

## egui Re-export {#260730-egui-reexport}

The crate re-exports egui itself, so callers write `gdext_egui::egui::...` rather than depending
on egui separately. This guarantees the caller's egui types are the exact ones the bridge was
compiled against — a separate dependency at a mismatched version would produce types that look
identical but do not interoperate.

`ViewportBuilder` and `ViewportId` are additionally re-exported at the crate root, since viewport
spawning takes both, and `EguiBridge` is the crate-root name of the bridge node type.

## Geometry Conversions {#260730-to-counterpart-conversions}

A conversion trait bridges Godot and egui geometry types. Each type has two conversions: its
natural counterpart, and an alternative for the cases where the same numbers mean something
different — a Godot `Vector2`, for example, converts to an egui vector or to an egui position
depending on which is asked for.

| From | Counterpart | Alternative |
|---|---|---|
| `Vector2` | `egui::Vec2` | `egui::Pos2` |
| `Vector2i` | `egui::Vec2` | `egui::Pos2` |
| `egui::Vec2` | `Vector2` | `Vector2i` |
| `egui::Pos2` | `Vector2` | `Vector2i` |
| `Rect2` | `egui::Rect` | `egui::Rect` |
| `egui::Rect` | `Rect2` | `Rect2i` |

Conversions to integer types truncate toward zero rather than rounding, so a position of 1.9
becomes 1.

> [!note] Implementation Gap · 2026-07-30
> Missing behavior. Converting an `egui::Rect` to `Rect2` or `Rect2i` produces the wrong size: the
> rectangle's maximum corner is written into the size field instead of the extent between the
> corners. The result is only correct for rectangles whose minimum corner is at the origin. The
> opposite direction, `Rect2` to `egui::Rect`, is correct, so a round trip does not survive. The
> crate's own use of this direction reads only the position, which is why the defect is not
> currently visible in the bridge's own behavior.

## Weak Reference Helpers {#260730-weakref-helpers}

Two helpers wrap Godot's weak reference mechanism with the typing callers want: one downgrades a
typed Godot handle to a weak reference, the other attempts to recover the typed handle from it,
yielding nothing when the object has been freed or is of a different type.

These exist so that callbacks can hold onto a Godot object without keeping it alive — the pattern
the bridge itself uses for root region sync, and the one callers need for widget callbacks that
outlive their owner.

## Drag and Drop Payload {#260730-drag-and-drop-variant}

egui's drag-and-drop utility requires payload types to be shareable across threads, which a Godot
`Variant` is not. The crate provides a wrapper that carries a `Variant` as an egui drag-and-drop
payload, constructed from a `Variant` and unwrapped back to one on the receiving side. This is
the payload type a Godot-originated drop delivers into egui.

The wrapper asserts thread-shareability that the underlying `Variant` does not have. It is sound
only because everything in this crate stays on the main thread; moving one to another thread and
touching it there is undefined behavior. Payloads are also expected to be cheap to clone —
handles and identifiers rather than bulk data.
