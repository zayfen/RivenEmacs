# AGENTS.md - RivenEmacs Development Guide

This document provides guidance for AI agents working on RivenEmacs, a highly modular and performance-optimized Emacs configuration.

## Build/Lint/Test Commands

```bash
# Check configuration syntax (all files)
emacs --batch -l init.el --eval "(message \"Config loaded successfully\")"

# Byte compile a single file
emacs --batch -f batch-byte-compile lisp/init-config.el

# Byte compile all configuration files
emacs --batch -f batch-byte-compile lisp/*.el

# Load init.el and test basic functionality
emacs --batch -l init.el --eval "(message \"All modules loaded\")"

# Test with clean state (no custom.el)
emacs -q -l ~/.emacs.d/init.el
```

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
(defun lsp-bridge-find-def-ex ()
  "Try lsp-bridge-find-def, with xref fallback."
  (interactive)
  (condition-case err
      (call-interactively 'lsp-bridge-find-def)
    (error (call-interactively 'xref-find-definitions))))
```

For optional features, check feature availability:
```elisp
(when (featurep 'agent-shell)
  (with-eval-after-load 'agent-shell ...))
```

### Keybinding Patterns
Uses `general.el` with leader key (SPC):
```elisp
(leader-def
  :infix "b"
  "" '(:ignore t :wk "Buffer")
  "." '(+show-current-buffer-path :wk "Show buffer path"))

(keymap-global-set "M-." #'lsp-bridge-find-def)
(keymap-global-set "C-," #'lsp-bridge-find-def-return)
```

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

## Module Loading Order

1. **Core**: `init-use-package` → `init-config` → `init-default` → `init-helper`
2. **UI**: `init-theme` → `init-font` → `init-undo` → `init-autosave`
3. **Foundation**: `init-which-key` → `init-general` → `init-hydra`
4. **Completion**: `init-consult` → `init-vertico` → `init-crux` → `init-editor`
5. **File/Edit**: `init-dired` → `init-format` → `init-jump` → `init-editorconfig`
6. **Code**: `init-checker` → `init-pair` → `init-fold` → `init-markdown`
7. **Parser**: `init-treesit`
8. **IDE**: `init-lsp-bridge`
9. **VCS**: `init-vc` → `init-debugger` → `init-git-hunk`
10. **Env/Project**: `init-envrc` → `init-project`
11. **AI**: `init-gpt` → `init-agent-shell`
12. **Languages**: `init-web` → `init-rust` → `init-python` → `init-java` → `init-swift`
13. **Writing**: `ews` (org hook), `init-org`
14. **Tools**: `init-docker` → `init-quickrun` → `init-feed` → `init-lookup` → `init-terminal` → `init-reader`
15. **Final**: `init-keybindings`

## Environment Variables

- `DEFAULT_WORKSPACE`: Default project directory
- `HTTP_PROXY`/`HTTPS_PROXY`: Network proxy
- `GROQ_API_KEY`, `DEEPSEEK_API_KEY`, `ANTHROPIC_AUTH_TOKEN`: AI services
