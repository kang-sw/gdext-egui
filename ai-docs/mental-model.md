# gdext-egui Mental Model

Cross-domain contracts and routing. Project summary, stack, and build commands live in
`ai-docs/_index.md`; current caller-visible behavior lives in `ai-docs/spec/`.

## Domains

| Domain | Covers |
|---|---|
| [frame-lifecycle](mental-model/frame-lifecycle.md) | The frame claim, the ROOT pass window, `finish_frame` phase order |
| [viewport-lifecycle](mental-model/viewport-lifecycle.md) | The validate/start/end triple, the split viewport/surface tables, close negotiation, the lock that deadlocks |
| [widget-callbacks](mental-model/widget-callbacks.md) | The two draw-callback registries, retain/dispose, expiry sentinels |
| [surface-rendering](mental-model/surface-rendering.md) | Canvas-item pooling, clip shader, texture library, UI scale |
| [input-routing](mental-model/input-routing.md) | Godot-to-egui event translation, routing modes, consumption |

## Crate Graph

This crate is a **library only** - it contains no GDExtension entry point. The consumer's
`cdylib` links it and gdext registers the classes through inventory, so the coupling runs one
way: consumer -> `gdext-egui`. Nothing here reaches back into the consumer except through
callbacks the consumer registered.

egui, however, *does* reach back into the bridge, through two hooks installed at frame start:
the repaint callback and the immediate-viewport renderer. Both are the reason several
apparently-local changes have global consequences - see `frame-lifecycle`.

Internally the dependency runs `context` -> `surface`, never the reverse. The painter never
holds a handle to the bridge; it talks back only through an event channel handed to it at
construction and through repaint requests on the shared context.

## Shared Conventions

**Single-threaded, proven only by comment.** The bridge carries a non-`Send` marker, but that
marker protects only the bridge struct. Two `unsafe impl Send` blocks - the spawned-viewport
callback wrapper and the drag-and-drop payload - defeat it deliberately, because egui requires
`Send + Sync` on both. Their soundness rests entirely on "nothing in this crate leaves the main
thread" {#260730-main-thread-contract} {#260730-drag-and-drop-variant}. Any change that makes a
deferred viewport callback or a drag payload reachable from a worker thread turns a comment into
undefined behavior - Godot refcount races and cross-thread `Variant` drops, not clean panics.

**Every frame entry point requires tree membership.** `current_frame`, `viewport_immediate`, and
`viewport_spawn` all reach code that expects the bridge to already be a child in the scene tree,
and panic otherwise. The doc examples on two of those methods show allocate-then-call sequences
that would panic {#260730-bridge-tree-lifecycle}.

**One Godot-callable method, bound by macro.** The single `#[func]` on the bridge exists only as
a deferred-call target, and its name is produced by a macro that fails to compile if the method
is renamed. Renaming it in one place breaks the build rather than the runtime - deliberately
{#260730-godot-egui-bridge-node}.

**Nothing is testable headlessly.** Every code path touches Godot handles, the rendering server,
or the display server, and the crate has no tests. Verification of rendering, input, or viewport
behavior means running `example/Showcase.tscn` in a Godot editor - say so and ask rather than
claiming a change is verified.

**Spec cross-references.** Domain docs embed spec anchors inline as `{#YYMMDD-slug}`. Grep those
across `ai-docs/mental-model/` to find which domain documents a given spec entry. Spec files
carry no back-references.

## Reading Map

| Task or topic | Read first | Then |
|---|---|---|
| Frame timing, repaint scheduling, "why isn't it drawing" | `spec/bridge-lifecycle.md` | `mental-model/frame-lifecycle.md` |
| Spawning windows, close behavior, viewport commands | `spec/viewport.md` | `mental-model/viewport-lifecycle.md`, then `frame-lifecycle` |
| Registering persistent UI, widget lifetime | `spec/widget-callbacks.md` | `mental-model/widget-callbacks.md` |
| Anything visual - meshes, clipping, textures, cursor | `spec/rendering.md` | `mental-model/surface-rendering.md` |
| Events, keys, clipboard, drag and drop | `spec/input.md` | `mental-model/input-routing.md` |
| Conversions, weak handles, the egui re-export | `spec/interop-helpers.md` | this file's Shared Conventions |
| Any change touching two of the above | this file | the two domain docs, then `spec/` for caller-visible impact |

Tickets: none exist yet. Use `git log --grep=<ticket-stem>` and read `## Ticket Updates` in
commit bodies once they do.
