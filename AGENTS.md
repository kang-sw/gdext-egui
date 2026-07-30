# AGENTS.md - gdext-egui

## Project Memory

Read at every session start, before other action:

1. **Preamble** - read `ai-docs/_index.md`; keep only context a session must not re-derive.
2. **Local** - read `ai-docs/_index.local.md` if present; it is .gitignored machine context.
3. **Project arc** - run `git log --oneline --graph -50`.

## Response Discipline

- **Evidence before claims.** Run verification and read output before stating success.
- **No performative agreement.** Restate the requirement, verify, then act or push back.
- **Actions over words.** Prefer "Fixed. [what changed]" or the diff. Skip filler.

## Code Standards

<!-- Project-wide code quality rules. -->

1. **Simplicity.** Write the simplest complete implementation that satisfies the spec.
2. **Surgical changes.** Change only what the task requires; follow existing style.
3. **Responsibility check.** Keep module roles clean; split when responsibility drifts.
4. **Testability.** Prefer explicit dependencies, minimal hidden state, pure logic over side effects.
5. **Read before change.** Read the existing code before proposing or making a change; no extra features, abstractions, or refactors beyond what was requested.

## Workflow

### Approval Protocol

- **Auto-proceed:** bug fixes, pattern-following additions, tests, boilerplate, single-module refactors.
- **Ask first:** new components/protocols, architecture changes, cross-module interfaces, observable behavior changes.
- **Always ask:** deleting functionality, changing protocol/API semantics, modifying persistence schema.

### Implementation

- Before writing code, briefly outline the approach: affected files, strategy, success criteria. A one-liner suffices for trivial changes.
- When introducing a new concept or subsystem, update the owning `ai-docs/` document first, then write code to match. Documentation leads implementation.

### Testing

- Write test cases aggressively for all logic where feasible.
- When a test fails, diagnose whether the **test assumption** or the **logic under test** is wrong, and fix the correct side. Do not edit a test just to make it pass.
- When verification requires user interaction (Godot editor, running scene, manual CLI), say so and request manual testing rather than skipping or faking it.

### Commit Rules

Auto-create one commit per logical unit. Include `## AI Context` explaining why the approach was chosen.

```text
<type>(<scope>): <summary>

<what changed - brief>

## AI Context
- <decision rationale, rejected alternatives, user directives, etc.>

## Ticket Updates                          # optional - ticket-driven only
- <ticket-stem>[: <optional-label>]
  > Forward: <future-phase finding>

## Spec                                    # optional - omit when none
- <spec-stem>
```

When a spec heading `{#slug}` changes, include `renamed-spec: <old-stem> -> <new-stem>`.

### Context Window Discipline

- Source code is ground truth; load only docs relevant to the task.
- Update drifted docs on contact.

## Architecture Rules

<!-- Project-wide invariants the AI must never violate. -->

1. **Single-threaded core.** `EguiBridge` and `EguiViewportBridge` run on the Godot main thread only; `_non_send_sync: PhantomData<*const ()>` enforces this. Do not introduce cross-thread access to egui or Godot object handles.
2. **No Godot calls under the viewport lock.** `viewport_validate` holds `share.viewports` across command application and info sync. A Godot call that synchronously emits a signal re-enters egui's repaint callback, which takes that same non-reentrant lock and self-deadlocks. Any new engine call reachable from there must go through `DeferredCommand` instead of being made inline. See `ai-docs/mental-model/viewport-lifecycle.md`.

<!-- Optional for GUI/TUI projects:
1. **Headless-testable architecture.** Domain logic and state live in framework-agnostic layers testable without a display. UI layers stay thin: no branching logic, state ownership, or domain knowledge.
-->

## Project Knowledge

- Project state and cross-session context live in `ai-docs/`.
- Workflow shape and plugin-less maintenance guidance live in `ai-docs/WORKFLOW.md`; it is explanatory and does not override ws runtime or MCP parser behavior.
- Before creating or editing tickets, load the write-ticket workflow skill for conventions.
- Reference tickets by stem only, never full path; stems survive status moves.
- To check ticket completion or prior phase results, use `git log --grep=<ticket-stem>` and inspect `## Ticket Updates`.
- Claude Code compatibility is `CLAUDE.md` containing `@AGENTS.md`.
- **Language:** AI-authored docs, plans, commits, tickets, and code comments are English. Human-facing UI strings are exempt.

<!-- Inclusion test: if breaking this rule makes a skill produce wrong results
     AND it applies everywhere, keep it here. Domain-scoped rules belong in
     `ai-docs/mental-model/<domain>.md ## Domain Rules` via `ws:lead-add-rule`.
     Context goes in `_index.md`; process goes in skills. -->

<!-- Template Version: v0045 -->
