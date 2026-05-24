# Startup Loading Hygiene Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make startup package loading deterministic and remove remaining unnecessary eager loads.

**Architecture:** Package initialization becomes an explicit early step so installed ELPA packages are discoverable in batch and interactive sessions. Remaining `general.el` usage moves into the declarative keybinding engine, and user-facing commands from editor, terminal, and agent modules are registered via autoloads instead of requiring heavy implementation modules during startup.

**Tech Stack:** Emacs Lisp, `use-package`, package.el, ERT batch tests.

---

### Task 1: Startup Package Initialization Regression

**Files:**
- Modify: `tests/test-focused-config-simplification.el`
- Modify: `early-init.el`

- [ ] **Step 1: Add tests for package initialization and package visibility**

Add ERT coverage that loads `early-init.el` in a controlled environment and verifies package initialization plus representative installed package lookup.

- [ ] **Step 2: Run focused tests and confirm the new startup test fails**

Run:

```bash
emacs --batch -Q -L lisp/core -L lisp/keymaps -L lisp/session \
  -l tests/test-init-session.el \
  -l tests/test-focused-config-simplification.el \
  -f ert-run-tests-batch-and-exit
```

Expected: the package initialization test fails because `early-init.el` currently leaves `package--initialized` nil on modern Emacs.

- [ ] **Step 3: Explicitly initialize packages in `early-init.el`**

Replace the Emacs 27 compatibility-only package initialization branch with an idempotent `(package-initialize)` call after `package-user-dir` and `package-archives` are configured.

- [ ] **Step 4: Run focused tests and confirm package visibility passes**

Run the same ERT command. Expected: all tests pass.

### Task 2: Remove Remaining `general.el` Startup Dependency

**Files:**
- Modify: `tests/test-focused-config-simplification.el`
- Modify: `init.el`
- Modify: `lisp/ide/init-debugger.el`
- Delete: `lisp/ui/init-general.el`

- [ ] **Step 1: Add tests proving `general` is no longer part of runtime wiring**

Add assertions that `init.el` does not require `init-general`, `init-debugger.el` does not call `leader-def`, and `lisp/ui/init-general.el` no longer exists.

- [ ] **Step 2: Run focused tests and confirm the new test fails**

Run the focused ERT command. Expected: failure while `init-general.el` and `leader-def` still exist.

- [ ] **Step 3: Remove `init-general` from startup and migrate debugger prefix**

Delete the single `leader-def` debugger registration from `init-debugger.el`, remove `(require 'init-general)` from `init.el`, and delete `lisp/ui/init-general.el`.

- [ ] **Step 4: Run focused tests and confirm they pass**

Run the focused ERT command. Expected: all tests pass.

### Task 3: Reduce Eager Command Module Loading

**Files:**
- Modify: `tests/test-focused-config-simplification.el`
- Modify: `lisp/editor/init-editor.el`
- Modify: `lisp/tools/init-terminal.el`
- Modify: `lisp/ai/agent-shell/init-agent-shell.el`
- Modify: `lisp/ai/agent-shell/init-agent-shell-commands.el`

- [ ] **Step 1: Add tests for lazy command declarations**

Add tests that verify `iedit`, `sudo-edit`, and `vterm` are declared with command-based loading, and that `init-agent-shell.el` no longer eagerly requires install/commands modules.

- [ ] **Step 2: Run focused tests and confirm the lazy loading tests fail**

Run the focused ERT command. Expected: failures for current eager declarations.

- [ ] **Step 3: Update module declarations**

Change `iedit` to command-based loading, add commands for `sudo-edit` and `vterm`, and make `init-agent-shell.el` autoload setup/start helper commands rather than requiring install/commands during startup. Ensure command files load their dependencies explicitly when invoked.

- [ ] **Step 4: Run focused tests and confirm they pass**

Run the focused ERT command. Expected: all tests pass.

### Task 4: Final Verification

**Files:**
- Modify: `docs/startup-benchmark.md`
- Modify: `docs/startup-optimization-report.md`

- [ ] **Step 1: Run focused ERT**

Run the focused ERT command. Expected: all tests pass.

- [ ] **Step 2: Run config load**

Run:

```bash
emacs --batch -l init.el --eval "(message \"Config loaded; package-init=%S general=%S agent-shell=%S vterm=%S\" (bound-and-true-p package--initialized) (featurep 'general) (featurep 'agent-shell) (featurep 'vterm))"
```

Expected: exit 0, `package-init=t`, and heavy optional features remain unloaded.

- [ ] **Step 3: Run diff hygiene**

Run:

```bash
git diff --check
```

Expected: no output.

- [ ] **Step 4: Update startup docs**

Record the package initialization and lazy-loading behavior in the startup docs.
