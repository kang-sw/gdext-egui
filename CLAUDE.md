# CLAUDE.md

## Documentation Layout

- All AI-readable documentation lives under `ai-docs/`. Do NOT store docs elsewhere.
- All `ai-docs/` documents, plans, and plan statements must be written in **English**, regardless of conversation language.

### Mental Model

- `ai-docs/mental-model.md` is the **project brief**: high-level goals, architecture overview, recent work log, and short/mid-term context.
- `ai-docs/mental-model/` is a directory for **deep technical details**:
  - Per-module or per-subsystem design documents
  - Technical implementation specs and constraints
  - Decision logs: what was decided, what was tried and reverted, and why
  - Workarounds and non-obvious technical choices that future sessions must remember
  - Link to these documents from the root `mental-model.md` where relevant.

### Project Files

- When implementing a complex feature, create a project file at `ai-docs/projects/YYMMDD-HHMM-<project-name>.md`.
- While work is in progress, append `[wip]` to the filename: `YYMMDD-HHMM-<project-name>[wip].md`. Remove the suffix when the project is complete.
- Each project file should contain:
  - Implementation spec and design goals
  - Summary of discussions and decisions leading to this work
  - Session-by-session milestone breakdown with detailed goal items per session
  - Technical implementation ideas and notes (when sufficiently clear)
- **Plan mode tagging**: For each milestone/phase, assess its complexity.
  - If a phase requires careful architectural thought, non-trivial design, or coordination across multiple modules, append `(plan mode)` to the phase title. This signals that you must enter plan mode and produce a detailed plan before writing any code for that phase.
  - If a phase is straightforward (small scope, clear implementation path), omit the tag and proceed directly.
  - Example:
    ```
    ### Phase 1: Database schema migration (plan mode)
    ### Phase 2: Add unit tests for validators
    ### Phase 3: Refactor event pipeline (plan mode)
    ```
- Use these files to track progress across sessions and maintain continuity.

## Workflow Rules

### Session Start

1. Read `ai-docs/mental-model.md` to understand current project state.
2. Check recent git history (`git log`) to catch up on work since the last mental-model update.
3. Load only the docs relevant to the current task — do not front-load the entire `ai-docs/` tree.

### Session End

1. Update `ai-docs/mental-model.md` with work done, current status, and near-term plans.
2. **Prune aggressively**: remove or condense entries that are no longer relevant. Old completed tasks, resolved issues, and obsolete context should be deleted or collapsed into a one-line summary. The file must stay concise and current.
3. Commit the mental-model update.

### Approval Protocol

- **Auto-proceed**: Bug fixes, pattern-following additions, test code, boilerplate, refactoring within a single module.
- **Ask first**: New component additions, architectural changes, cross-module interface changes, anything that alters user-facing behavior.
- **Always ask**: Deleting existing functionality, changing protocol/schema semantics, modifying persistence or deployment.

### Implementation

- Before writing code, briefly outline the approach: affected files, strategy, and success criteria. For trivial changes a one-liner suffices.
- Read existing code before proposing or making changes.
- Keep solutions minimal — no extra features, abstractions, or refactors beyond what is explicitly requested.
- When implementing a new concept or subsystem, **update the relevant `ai-docs/` documentation first**, then write the code to match. Documentation leads implementation.
- **Update `ai-docs/mental-model.md` at every meaningful checkpoint** — not just at session end. Treat it as a living working-memory document.

## Commit Rules

- Create a commit after every unit of work, including documentation changes.
- Every commit message must include an `## AI COMMENT` section at the end:
  - Design decisions made and alternatives considered
  - Key discussion points and rationale
  - Context that helps a future AI reconstruct the working state from commit history

## Testing Rules

- Write test cases aggressively for all logic where feasible.
- When a test fails, diagnose whether the **assumption in the test** is wrong or the **logic under test** is buggy. Fix the correct side — do not blindly modify the test to make it pass.
- If testing requires user interaction (e.g., UI, manual CLI invocation), notify the user and request manual testing rather than skipping or faking it.

## Project-Specific Notes

*(Populated as the project evolves — see `ai-docs/mental-model.md` for current state.)*
