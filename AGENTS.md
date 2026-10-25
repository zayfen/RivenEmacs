# AGENTS.md - RivenEmacs Development Guide

This document provides guidance for AI agents working on RivenEmacs, a highly modular and performance-optimized Emacs configuration (target: Emacs 31.1).

## Build/Lint/Test Commands

`scripts/run-tests.sh` is the canonical test entry point. It builds `load-path`
from the repository itself, prefers the checked-in sources over stale `.elc`
files, never loads `init.el`, and needs no network:

```bash
# Run everything: project ERT + emacs-codeforces ERT + Python agent unit tests
scripts/run-tests.sh

# Run a single ERT file (bare names are resolved in tests/ and emacs-codeforces/test/)
scripts/run-tests.sh tests/test-init-terminal.el
scripts/run-tests.sh emacs-codeforces/test/emacs-codeforces-auth-test.el

# Skip the Python unit tests
scripts/run-tests.sh --no-python

# Python agent unit tests only (offline; bs4-only tests are skipped when missing)
python3 -m unittest discover -s emacs-codeforces/test -v

# Opt-in: also run the startup integration tests that spawn `emacs -l init.el`
# (slow, may install packages over the network)
RIVENEMACS_RUN_INIT_TESTS=1 scripts/run-tests.sh

# Upgrade ELPA packages and package-vc (Git) checkouts; dry-run first
scripts/update-packages.sh --dry-run
scripts/update-packages.sh

# Byte compile a single module (configuration modules live in lisp/<group>/)
emacs --batch -f batch-byte-compile lisp/core/init-config.el

# Interactive smoke test; may install packages from ELPA and access the network
emacs -q -l /path/to/RivenEmacs/init.el
```

`emacs --batch -f batch-byte-compile lisp/*.el` is obsolete — `lisp/` only
contains `env.el` at the top level and all modules live in `lisp/<group>/`.

## Code Style Guidelines

### File Headers
```elisp
;; -*- coding: utf-8; lexical-binding: t -*-

;;; init-example.el --- Brief description

;;; Commentary:
;; Detailed explanation

;;; Code:
```

### Indentation & Formatting
- **Tab width**: 2 spaces
- **Fill column**: 120 characters
- **Never use tabs**: `indent-tabs-mode nil`
- **Always use lexical-binding**
- **Line spacing**: 0.25

### Naming Conventions
| Pattern | Example | Usage |
|---------|---------|-------|
| Public config | `rivenEmacs-*` | `rivenEmacs-default-workspace` |
| Public functions | `riven/*` | `riven/google-search` |
| Internal helpers | `+function-name` | `+directory-ensure` |
| Module prefixes | Consistent with filename | `init-config.el` → `provide 'init-config` |

### Import Patterns
```elisp
;; Load core modules (order matters - dependencies first)
(require 'init-use-package)
(require 'init-config)
(require 'init-default)
(require 'init-helper)

;; use-package for external packages
(use-package package-name
  :ensure t              ; Auto-install if missing
  :defer 2               ; Defer loading by 2 seconds
  :custom (package-option value)
  :hook ((mode . hook-fn))
  :bind (:map mode-map ("key" . function))
  :config (progn ...))
```

### Error Handling
Use `condition-case` for graceful fallbacks:
```elisp
(defun riven/find-definition ()
  "Try Eglot's definition handler, falling back to plain xref."
  (interactive)
  (condition-case err
      (call-interactively #'eglot-find-declaration)
    (error (call-interactively #'xref-find-definitions))))
```

For optional features, check feature availability:
```elisp
(when (featurep 'ai-code)
  (with-eval-after-load 'ai-code ...))
```

### Keybinding Patterns
RivenEmacs uses a **declarative keybinding engine with `C-c` as the leader
prefix**. `general.el` and the old `SPC` leader are retired; do not add
`leader-def` forms or `general-*` dependencies.

Keybindings are declared in specs, not inline in feature modules:

```elisp
;; lisp/keymaps/keybindings-spec-core.el
(defconst riven/keybindings-leader-spec-core
  '(("f" "File"
     (("f" consult-fd "Find file")
      ("r" consult-recent-file "Recent file")))))
```

The engine (`lisp/keymaps/keybindings-engine.el`) applies every spec on
`after-init-hook`, clears the `C-c` prefixes it owns, registers which-key
labels, and warns about duplicate keys / missing commands. Add commands that
back a spec to `lisp/keymaps/keybindings-commands.el`.

Global bindings that are not part of a leader group are set directly, e.g.
`(keymap-global-set "M-." #'riven/xref-find-definitions-or-search)`.

### Documentation
Every function needs a docstring. Use `defcustom` for user-configurable options:
```elisp
(defcustom rivenEmacs-default-workspace
  (expand-file-name "~/")
  "Default workspace directory for RivenEmacs."
  :type 'directory
  :group 'rivenEmacs)
```

### Module Structure
1. File header with coding declaration
2. Commentary section
3. `;;; Code:` marker
4. Imports (`require`, `use-package`)
5. Configuration
6. Keybindings
7. `provide 'module-name`
8. `;;; filename.el ends here`

### Common Patterns
- Use `local-dir` and `cache-dir` for per-config directories
- Always use `expand-file-name` for path construction
- Check `(featurep 'feature)` before requiring optional modules
- Use `interactive` for user-callable functions
- Prefer `condition-case` over silent error suppression

## Repository Layout

```
init.el / early-init.el      # assembly + startup performance only
lisp/core/                   # env, config, defaults, helper, use-package
lisp/ui/                     # theme, font, which-key, hydra
lisp/completion/             # minibuffer/vertico stack, consult, corfu, cape
lisp/editor/                 # editing, format, pairs, fold, undo, markdown
lisp/files/                  # dired/dirvish
lisp/parser/                 # tree-sitter
lisp/ide/                    # eglot (only LSP client), checker, debugger, jump, envrc, project
lisp/vcs/                    # magit, diff-hl, git-timemachine, smerge
lisp/ai/                     # gptel, MCP, ai-code, dsh-emacs
lisp/lang/                   # per-language config
lisp/keymaps/                # declarative keybinding specs + engine
lisp/session/                # desktop.el session persistence
lisp/tools/                  # terminal, docker, quickrun, feed, lookup, reader, codeforces
lisp/writing/                # ews, beamer lecture export
emacs-codeforces/            # bundled Codeforces client (Elisp + Python agent)
tests/                       # project ERT suite
scripts/                     # riven-deps.sh (deps), run-tests.sh (tests),
                             # update-packages.sh (ELPA/VC upgrades)
```

`lisp/custom.el` was removed: `custom-file` points to `custom.el` at the
repository root (set in `early-init.el`). Keep Custom-generated state there and
do not recreate `lisp/custom.el` (it would shadow the built-in `custom`).

## Module Loading Order

`init.el` only assembles; feature configuration lives in the modules.

1. **Bootstrap**: `load-path` (repo root, `lisp/`, every first-level subdir of
   `lisp/` plus its immediate subdirectories, except `lisp/local`),
   `exec-path-from-shell` (deferred 2 s), `lisp/env.el`, package system.
2. **Immediate modules** (in this order):
   - **Core**: `init-use-package` → `init-config` → `init-default` → `init-helper`
   - **UI/editing**: theme, font, undo, autosave, which-key, hydra, minibuffer,
     completion-ui, consult, vertico (shim), crux, editor, dired, format, jump,
     editorconfig, checker, pair, fold, markdown, treesit
   - **Writing**: `ews` (immediate) + `init-org` (lazy via `org-mode-hook`)
   - **Session**: `init-session`
   - **Tooling**: `init-terminal`
   - **Keymaps**: `init-keybindings` (registers its `after-init-hook` handler)
3. **Deferred modules** are registered on `after-init-hook` at **depth -20**,
   so they load during startup at the front of `after-init-hook`, before the
   default-depth handlers (desktop restore, keybinding application) and before
   the first redisplay. This is still synchronous startup work (not idle-deferred),
   so it adds to total startup time; `emacs-startup-hook`, where `early-init.el`
   restores the normal GC threshold, runs after them:
   `init-eglot` → `init-vc` → `init-debugger` → `init-envrc` →
   `init-project` → `init-gpt` → `init-ai-code` → `init-dsh-emacs` →
   `init-web` → `init-rust` →
   `init-python` → `init-java` → `init-swift` → `init-docker` →
   `init-quickrun` → `init-feed` → `init-lookup` → `init-reader` →
   `init-codeforces`.

## LSP / Terminals

- **LSP**: `eglot` is the only LSP client (see `lisp/ide/init-eglot.el`).
  `lsp-bridge` was removed; do not reference `lsp-bridge-*` in new code.
  `rivenEmacs-lsp-modes` in `lisp/core/init-config.el` lists auto-enabled modes.
  When a buffer looks managed but completion is missing, run
  `M-x riven/eglot-diagnose`.  Hover documentation appears when the mouse
  rests on a symbol (`riven/eglot-mouse-hover-mode`, `C-c c h` opens the full
  ElDoc buffer, `C-c c n`/`C-c c p` cycle signature overloads).
  rust-analyzer needs a Cargo project
  (`Cargo.toml`/`rust-project.json`); the bundled Codeforces client generates
  a `Cargo.toml` with `[[bin]] path = "solution.rs"` for Rust workspaces.
- **Terminal**: `ghostel` is the primary terminal (`C-c t t`); `eat` and
  `vterm` are declared as fallbacks (`lisp/tools/init-terminal.el`).

## Environment Variables

- `DEFAULT_WORKSPACE`: Default project directory
- `HTTP_PROXY`/`HTTPS_PROXY`: Network proxy
- `GROQ_API_KEY`, `DEEPSEEK_API_KEY`, `OPENAI_API_KEY`, `ANTHROPIC_AUTH_TOKEN`,
  `ANTHROPIC_BASE_URL`: AI services
- `BRAVE_API_KEY`, `TAVILY_API_KEY`, `GITHUB_TOKEN`: MCP tool servers
- `CODEFORCES_HOME`: Optional override of the Codeforces client home directory
  (Emacs passes it to the bundled Python agent automatically)

See `scripts/riven-deps.sh list` for the authoritative dependency/env list.
Docs live in `docs/wiki/`; start with `docs/wiki/Development.md`.
