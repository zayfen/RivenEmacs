# Development

How to work on RivenEmacs itself. The authoritative conventions source is
`AGENTS.md` at the repo root; this page summarizes and extends it.

---

## Build / Lint / Test

```bash
# Check configuration syntax (all files)
emacs --batch -l init.el --eval "(message \"Config loaded successfully\")"

# Byte compile a single file
emacs --batch -f batch-byte-compile lisp/ide/init-eglot.el

# Byte compile all configuration files
emacs --batch -f batch-byte-compile lisp/*.el

# Load init.el and test basic functionality
emacs --batch -l init.el --eval "(message \"All modules loaded\")"

# Test with clean state (no custom.el)
emacs -q -l ~/.emacs.d/init.el

# Run the ERT test suite
emacs --batch -l ert -l tests/*.el -f ert-run-tests-batch-and-exit

# Benchmark startup
emacs -Q --batch -l benchmark-startup.el
```

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
[Architecture](Architecture.md#loader--module-mapping)):

1. **Core**: `init-use-package` → `init-config` → `init-default` → `init-helper`
2. **UI**: `init-theme` → `init-font` → `init-undo` → `init-autosave`
3. **Foundation**: `init-which-key` → `init-hydra`
4. **Completion**: `init-consult` → `init-minibuffer` → `init-completion-ui` → `init-crux` → `init-editor`
5. **File/Edit**: `init-dired` → `init-format` → `init-jump` → `init-editorconfig`
6. **Code**: `init-checker` → `init-pair` → `init-fold` → `init-markdown`
7. **Parser**: `init-treesit`
8. **IDE**: `init-eglot`
9. **VCS**: `init-vc` → `init-git-hunk` → `init-debugger`
10. **Env/Project**: `init-envrc` → `init-project`
11. **AI**: `init-gpt` → `init-ai-code`
12. **Languages**: `init-web` → `init-rust` → `init-python` → `init-java` → `init-swift`
13. **Writing**: `ews` (org hook), `init-org`
14. **Tools**: `init-docker` → `init-quickrun` → `init-feed` → `init-lookup` → `init-terminal` → `init-reader`
15. **Final**: `init-keybindings`

Heavy modules (steps 8–14) go in `riven/load-deferred-modules` (load on
`emacs-startup-hook`); the rest load immediately.

---

## Adding a Keybinding

RivenEmacs uses a **declarative keybinding engine** — do **not** bind keys
inline in modules. Instead, add a spec to the appropriate file in
`lisp/keymaps/`:

```elisp
;; lisp/keymaps/keybindings-spec-core.el
("p s" consult-ripgrep "ripgrep in project")
```

Each spec is a `(key command which-key-label)` triple. The engine:

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

Tests live in `tests/` and use ERT. Run with:

```bash
emacs --batch -l ert -l tests/<test-file>.el -f ert-run-tests-batch-and-exit
```

Add tests for any non-trivial logic (the keybinding engine, server resolution,
Codeforces client, etc.).
