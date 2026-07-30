---
domain: widget-callbacks
description: "The two priority-ordered draw-callback registries, the retain/dispose contract they share with spawned viewports, and the decorator/sentinel extension points."
sources:
  - src/context/
related:
  frame-lifecycle: "Both registries are invoked at fixed points inside the ROOT pass; registration also sets the repaint wake flag."
  viewport-lifecycle: "WidgetRetain is shared between the two, and Unspecified is read differently on each side."
---

# Widget Callbacks

## Entry Points

- `src/context/mod.rs` - the two registries, `impl_push_panel_item`, and `WidgetRetain`.
- `src/context/widget_traits.rs` - the `CheckExpired` sentinel trait and the decorators.
- `src/context/frame.rs` - `invoke_registered_callbacks` is the only consumer.

## Module Contracts

- **The sandwich invariant.** The "first" group runs immediately after the ROOT pass begins and
  the "last" group immediately before it ends {#260730-render-callback-registration}. That
  window is the only interval in which the ROOT pass is open, so a third invocation point has
  to go inside it.
- "Last" callbacks run **before** `finish_frame` collects spawned viewports, so a
  `viewport_spawn` issued from a last-callback gets its surface validated this frame. Its UI
  callback still first runs next frame: the drain loop decides scheduling from the viewport
  map *before* validation inserts the new entry.
- `Unspecified` means different things at the two viewport comparison sites in `finish_frame`.
  The first (collection) retains it; the second (disposal) treats anything but `Retain` as
  dispose, but is only reached once the viewport has stopped producing output. In practice a
  `()`-returning viewport callback lives indefinitely - which makes the source comment saying
  it "will be disposed at the end of frame" misleading {#260730-widget-retain-lifetime}.
  Adding a variant means auditing both sites, written as opposite tests of each other.
- Registration writes the repaint wake flag directly rather than starting a frame
  {#260730-registration-wakes-repaint}. Contrast `viewport_spawn`, which routes through the
  deferred frame-start path; widget registration deliberately does not open a frame at all.

## Coupling

- The retain enum is shared with `viewport-lifecycle`. Normalizing the two viewport
  comparisons to a single predicate flips the `()`-returning-callback case: either viewports
  never close, or they close on their first frame.
- The decorators all hard-return `Dispose` and never compose through `WidgetRetain::and`; the
  combinator is public API for consumers, not internal machinery.

## Extension Points & Change Recipes

- **New expiry sentinel**: implement `CheckExpired` {#260730-expiry-sentinels}. The three
  flag-shaped impls negate an alive-flag; the two weak-pointer impls test emptiness directly.
- **New decorator**: a default method on `FnEguiDrawExt`; additive, no impl-site changes.
- **(planned)** `src/widgets.rs` is an empty file exported as a public module, and the
  changelog advertises a widget-spawning API that does not exist in this tree.

## Common Mistakes

- **Expecting `()` to mean "one shot."** It converts to `Unspecified`, which for widgets is
  permanent retention. A closure with no tail expression registers forever - the usual source
  of leaked panels.
- **Expecting a handle to cancel a registration.** Disposal is purely the callback's own
  return value; nothing is handed back to the registrant. Binding a sentinel is the only
  external cancellation mechanism {#260730-draw-callback-decorators}.
- **`bind(true)` / `bind(false)`.** A plain `bool` sentinel is copied into the closure at
  decoration time and can never change afterwards - immortal or dead on arrival.
- **`lifespan` starts its clock when decorated, not when first drawn.** Decorating long before
  registering burns the budget.
- **Decorator order changes side effects, not lifetime.** Both `.once().bind(s)` and
  `.bind(s).once()` dispose after one invocation; the difference is that the former skips the
  user closure entirely when the sentinel is already expired, while the latter still runs it
  once.
- **Callbacks are not once per engine tick.** Because the bridge opens a frame whenever the
  repaint flag is set and user code can open another in the same tick, a "first" callback can
  run twice in one tick. Callbacks that mutate external counters per invocation will be wrong.
- **Binding the bridge from inside a callback.** A "last" callback always runs under `process`'s
  exclusive bind, so `bind()` there panics unconditionally. A "first" callback only panics on
  frames the bridge opened by itself - see `frame-lifecycle`.
- **Omitting the closure parameter type.** The draw trait is generic over the return type, so
  `|ctx: &egui::Context|` is required for inference when decorators are chained.

## Technical Debt

- **The Godot-handle sentinel has inverted polarity.** It reports expired while the object is
  valid and alive after it is freed, so binding a widget to a Godot handle disposes it on its
  first frame. It is the one `CheckExpired` impl that does not negate. No ticket exists.
- **Re-entrant registrations can be silently destroyed.** `invoke_registered_callbacks` takes
  the registry, runs the callbacks, then merges. The merge only appends-and-sorts when *both*
  halves are non-empty; otherwise it overwrites. So if every surviving callback disposed itself
  and a callback registered a replacement during the pass, the replacement is discarded with no
  error - concretely, a `once()` callback that re-registers itself never runs again. This
  contradicts the spec's claim that re-entrant registration skips no entries
  {#260730-reentrant-registration}.
- **Tie ordering is not stable, contrary to the spec.** Insertion uses a binary search, which
  returns an unspecified index among equal priorities, and the merge path's stable sort places
  newly registered callbacks *ahead* of retained peers at the same priority. Use distinct
  priorities.
- **A panicking callback destroys its whole group.** The registry is moved into a local before
  invocation, so when gdext catches the panic at the `process` boundary, that local - every
  callback in the group - is dropped. The game keeps running with all of those widgets gone.
