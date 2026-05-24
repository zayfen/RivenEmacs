# Focused Configuration Simplification Design

**Date:** 2026-05-24
**Repository:** RivenEmacs

## Goal

Simplify RivenEmacs configuration code by removing proven redundant or obsolete
connection points while preserving currently useful features. The change covers
both committed configuration and the user's current in-progress AI/Org work,
without reverting those additions or broadening into a full style rewrite.

## Scope

The simplification is limited to the project's configuration wiring:

- Make `ai-code` the primary AI coding menu entry while retaining `gptel` chat
  and MCP commands and retaining `agent-shell` as a usable backend/tool.
- Consolidate keybinding ownership so one declarative layer owns shared
  prefixes.
- Remove unreferenced compatibility shims where current loading and
  documentation do not depend on them.
- Keep desktop session behavior while eliminating duplicate binding
  declarations.
- Reduce demonstrably noisy or duplicated expressions in `init-org.el` without
  removing its optional features.

The work excludes:

- Third-party code such as `lisp/writing/ox-beamer-lecture.el`.
- Deleting optional functions whose retention is intentional, including LSP
  Bridge files and Org export/transclusion capabilities.
- Sweeping file-header, naming, or formatting normalization.
- Generated/state data such as `var/`, `mcp-memory.jsonl`, and Org clock state.

## Chosen Approach

Use a focused cleanup rather than a repository-wide normalization or module
split. This produces reviewable edits grounded in confirmed duplicate loading
or binding behavior and can be verified through existing batch-load and ERT
tests.

Alternatives not selected:

- A horizontal normalization pass would touch many modules for cosmetic
  consistency without comparable runtime or maintenance benefit.
- A deep split of `init-gpt.el` and `init-org.el` would be useful only with a
  broader module redesign and substantially larger verification surface.

## Module Changes

### AI Entry Points

`ai-code` remains configured from the deferred module loading phase and is the
primary coding-menu entry through `M-*` and its AI-group binding. `gptel`
commands remain available for console, rewrite, and MCP workflows.

`agent-shell` remains available for direct agent use and as an `ai-code`
backend. Remove the unused `riven/agent-shell-dispatch` function because
`ai-code-menu` has taken over its only prior global entry point.
`agent-shell-transient` stays available for explicit agent-shell workflows.

The current `agent-shell-setup` feature remains. Any simplification of its
installer flow must keep visible error reporting and must not silently swallow
network or package-update failures.

### Keybindings

The declarative keybinding specification and engine become the single source
for application keybindings. Keybinding initialization must not eagerly load
the complete `agent-shell` package merely to establish agent command bindings;
commands should remain lazy/autoloaded until invoked.

Delete the four unreferenced compatibility files below after a final
implementation-time reference check confirms that no new consumer has been
introduced:

- `lisp/keymaps/init-keybindings-core.el`
- `lisp/keymaps/init-keybindings-ai.el`
- `lisp/keymaps/init-keybindings-org.el`
- `lisp/keymaps/init-keybindings-session.el`

Compatibility shims or optional modules still referenced by startup or
documented as retained are not removed. This includes `init-vertico.el`,
`init-lsp-bridge.el`, and `init-git-hunk.el`.

### Desktop Sessions

Desktop save/load/delete behavior and user-facing session commands remain
unchanged. `init-session.el` no longer needs to create and globally install a
second `C-c s` prefix map when the declarative keybinding spec already binds
the same commands. Session keybinding tests should assert the final configured
bindings instead of relying on the removed duplicate local map variable.

### Org Configuration

`init-org.el` retains agenda/calendar integration, capture behavior, `org-ql`,
Beamer lecture export, weekly task reporting, and `org-transclusion`. Cleanup
is restricted to removing example/tutorial noise, combining repeated
assignments where clear, and using direct path construction without changing
features or hooks.

The in-progress named agenda advice functions are retained because they improve
traceability compared with anonymous advice and are within the approved
feature boundary.

## Error Handling And Safety

- Existing uncommitted work is treated as the working baseline; the cleanup
  must integrate with it and must not revert unrelated modifications.
- Optional integrations continue to degrade gracefully when their package or
  executable is absent.
- Installer/setup failures remain visible through `message` output and
  `condition-case` paths.
- Optional capabilities are preserved unless a reference check proves a
  connection point is unused and it falls within this design's explicit
  deletion list.

## Verification

Verification should cover the affected connection points rather than trigger
external installation or network updates:

1. Run Emacs batch initialization:

   ```bash
   emacs --batch -l init.el --eval "(message \"Config loaded successfully\")"
   ```

2. Run the session ERT suite after adapting its binding expectations.

3. Batch-load or byte-compile the touched Emacs Lisp modules where practical
   to catch parse errors, missing functions, and invalid wiring.

4. Add or run lightweight assertions confirming that `ai-code-menu` is the
   main configured AI coding entry and that deleted dispatcher/shim references
   do not remain.

No command that installs or upgrades agent packages is part of verification.

## Success Criteria

- `ai-code` is the primary coding-menu entry without forcing eager
  `agent-shell` package loading during keybinding setup.
- `gptel`, direct `agent-shell` actions, LSP Bridge files, and Org optional
  features remain available.
- Redundant session keymaps and unreferenced keybinding shims are removed or
  demonstrated to be required.
- Org cleanup reduces configuration noise without changing approved behavior.
- Batch initialization and targeted tests pass after the change.
