# Development

How to work on RivenEmacs itself. The authoritative conventions source is
`AGENTS.md` at the repo root; this page summarizes and extends it.

---

## Build / Lint / Test

```bash
# Run the offline test suites (project ERT + emacs-codeforces ERT, then the
# Python agent unit tests).  Never loads init.el and never needs the network.
scripts/run-tests.sh

# Run a single ERT file
scripts/run-tests.sh tests/test-init-terminal.el
scripts/run-tests.sh emacs-codeforces/test/emacs-codeforces-auth-test.el

# ERT only (skip the Python unit tests)
scripts/run-tests.sh --no-python

# Python agent unit tests only
python3 -m unittest discover -s emacs-codeforces/test -v

# Byte compile a single module
emacs --batch -f batch-byte-compile lisp/core/init-config.el

# Upgrade ELPA packages and package-vc (Git) checkouts; dry-run first
scripts/update-packages.sh --dry-run
scripts/update-packages.sh

# Interactive smoke test; may install packages from ELPA (network access)
emacs -q -l /path/to/RivenEmacs/init.el

# Benchmark startup (numbers in the reports are historical)
emacs -Q --batch -l benchmark-startup.el
```

> `batch-byte-compile lisp/*.el` is obsolete: configuration modules live in
> `lisp/<group>/`, so use the per-module form above (or an explicit file list).

---

## Updating Packages

`scripts/update-packages.sh` upgrades both kinds of installed packages without
loading `init.el`:

```bash
# See what would change (also refreshes the ELPA index and fetches VC remotes)
scripts/update-packages.sh --dry-run

# Upgrade everything, then restart Emacs to load the new versions
scripts/update-packages.sh

# Only one side / one package / offline
scripts/update-packages.sh --elpa-only
scripts/update-packages.sh --vc-only --only vertico
scripts/update-packages.sh --no-refresh --no-fetch
```

How each package kind is handled:

- **ELPA packages** — refreshed with `package-refresh-contents`, then upgraded
  with `package-upgrade` (the same operation as `M-x package-upgrade-all`
  without the confirmation prompt).
- **package-vc checkouts** — updated in place with synchronous Git commands,
  then rebuilt with `package-vc-rebuild` (autoloads, package metadata, byte
  code; native compilation is deferred to the next Emacs start):
  - branch checkouts are fast-forwarded to their upstream;
  - checkouts detached at a release commit (tagged or not) advance to the newest release commit
    (the last commit that changed the `Version:` header);
  - checkouts detached at the remote tip (use-package `:rev :newest`) advance
    to the new tip;
  - pinned commits and checkouts with tracked local changes are skipped and
    reported.
- VC packages that use `:make`/`:shell-command` build steps (for example
  `reader`) require `--allow-build`; build commands stay disabled otherwise.

The script never edits `init.el` or `custom.el` and returns non-zero if any
upgrade failed. `scripts/update-packages.sh --help` lists all options.

---

## Code Style

- **Indentation**: 2 spaces, `indent-tabs-mode nil`.
- **Fill column**: 120 characters.
- **Always use `lexical-binding: t`** (first line of every file).
- **Line spacing**: 0.25.

### File Header Template

```elisp
;; -*- coding: utf-8; lexical-binding: t -*-

;;; init-example.el --- Brief description

;;; Commentary:
;; Detailed explanation

;;; Code:
```

### Naming Conventions

| Pattern | Example | Usage |
|---------|---------|-------|
| Public config | `rivenEmacs-*` | `rivenEmacs-default-workspace` |
| Public functions | `riven/*` | `riven/google-search` |
| Internal helpers | `+function-name` | `+directory-ensure` |
| Module prefixes | match filename | `init-config.el` → `provide 'init-config` |

---

## Import Patterns

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
(defun riven/safe-find-def ()
  "Try lsp find-def, with xref fallback."
  (interactive)
  (condition-case err
      (call-interactively 'eglot-find-declaration)
    (error (call-interactively 'xref-find-definitions))))
```

For optional features, check feature availability:

```elisp
(when (featurep 'ai-code)
  (with-eval-after-load 'ai-code ...))
```

---

## Module Structure

Every module file follows this order:

1. File header with `lexical-binding` declaration
2. Commentary section
3. `;;; Code:` marker
4. Imports (`require`, `use-package`)
5. Configuration
6. Keybindings
7. `provide 'module-name`
8. `;;; filename.el ends here`

---

## Module Loading Order

When adding a new module, place its `require` in the right loader function (see
[Architecture](Architecture.md#loader--module-mapping)). `init.el` currently
uses:

1. **Core**: `init-use-package` → `init-config` → `init-default` → `init-helper`
2. **UI/editing**: `init-theme` → `init-font` → `init-undo` → `init-autosave`
   → `init-which-key` → `init-hydra` → `init-minibuffer` →
   `init-completion-ui` → `init-consult` → `init-vertico` (compatibility shim)
   → `init-crux` → `init-editor` → `init-dired` → `init-format` →
   `init-jump` → `init-editorconfig` → `init-checker` → `init-pair` →
   `init-fold` → `init-markdown` → `init-treesit`
3. **Writing**: `ews` (immediate) + `init-org` (lazy via `org-mode-hook`)
4. **Session**: `init-session`
5. **Tooling**: `init-terminal`
6. **Keymaps**: `init-keybindings` (registers its `after-init-hook` handler)
7. **Deferred**: `riven/load-deferred-modules` on `after-init-hook` at depth
   -20 — eglot, vc, debugger, envrc, project, gpt, ai-code, dsh-emacs, web,
   rust, python, java, swift, docker, quickrun, feed, lookup, reader,
   codeforces.
   The keybinding specs are then applied by the default-depth `after-init-hook`
   handler, so every deferred command exists before its spec is validated.

---

## Adding a Keybinding

RivenEmacs uses a **declarative keybinding engine** — do **not** bind keys
inline in modules. Instead, add an entry to the matching group in the right
spec file under `lisp/keymaps/`. A group is `(prefix title bindings)` and each
binding is a `(key command which-key-label)` triple:

```elisp
;; lisp/keymaps/keybindings-spec-core.el
;; Inside the ("p" "Project" ...) group:
("s" consult-ripgrep "ripgrep in project")
```

For a key that does not belong to a `C-c` group, use
`(keymap-global-set "M-." #'riven/xref-find-definitions-or-search)` in the
module that owns it (e.g. `lisp/ide/init-eglot.el`).

The engine:

- Clears owned `C-c` prefixes before applying.
- Registers which-key descriptions.
- **Warns at load time** on duplicate keys or missing commands.

For a command backing a spec, add it to `lisp/keymaps/keybindings-commands.el`.

---

## Adding Language Server Auto-Install

Use the generic helper in `lisp/ide/init-eglot.el`:

```elisp
(riven/eglot-register-simple-server
 '(my-lang-mode my-lang-ts-mode)
 "my-language-server"                      ; binary to detect
 '("my-language-server" "--stdio")         ; eglot contact list
 '("npm" "i" "-g" "my-language-server"))   ; install command
```

This registers a function-form contact that prompts to install on connect.
For multi-server priority chains, see `riven/eglot--pick-server`.

---

## Environment Variables

When you add a feature that needs an env var, also:

1. Add it to the `exec-path-from-shell-variables` list in `init.el`.
2. Document it in `scripts/riven-deps.sh` (the doctor/fix script).
3. Add it to [Customization](Customization.md).

---

## Committing

- The project uses **conventional commits** (`feat(scope): …`, `fix(scope): …`).
- Examples from history: `perf(codeforces): …`, `fix(codeforces): …`,
  `feat(eglot): install missing language servers on demand`.
- Don't commit on `main` directly — branch first, PR, then fast-forward merge.
- Never commit `elpa/`, `local/`, `eln-cache/`, or `custom.el` — they're gitignored
  or user-specific.

---

## Testing

Tests live in `tests/` (configuration modules) and
`emacs-codeforces/test/` (both the bundled client's ERT suite and Python
unit tests for `scripts/codeforces_agent.py`). Run everything with the
repository runner:

```bash
scripts/run-tests.sh
```

The runner adds the repository, every `lisp/` subdirectory, `tests/` and
`emacs-codeforces/` to `load-path`, prefers the checked-in sources over stale
`.elc` files, and never loads `init.el`. It exits non-zero if any suite fails.

- **Default**: all `tests/test-*.el` ERT files, all
  `emacs-codeforces/test/emacs-codeforces-*-test.el` files, and
  `python3 -m unittest discover -s emacs-codeforces/test`.
- **Single file**: `scripts/run-tests.sh <test-file>`.
- **Opt-in startup tests**: `RIVENEMACS_RUN_INIT_TESTS=1 scripts/run-tests.sh`
  additionally runs the tests that spawn a nested `emacs -l init.el` (slow and
  may install packages over the network).
- **Python only**:
  `python3 -m unittest discover -s emacs-codeforces/test -v`
  (bs4-dependent tests are skipped automatically when the optional dependency
  is missing).

Add tests for any non-trivial logic (the keybinding engine, server resolution,
the Codeforces client, the Python agent's pure helpers, etc.).
