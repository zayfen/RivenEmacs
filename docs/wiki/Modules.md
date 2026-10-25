# Modules Reference

All configuration lives under `lisp/`, organized into **15 module groups** by
concern. Each `.el` file follows the module template (see
[Development](Development.md)).

---

## `lisp/core/` — Foundation

| File | Purpose |
|------|---------|
| `env.el` | Path/environment constants: `root-dir`, `lisp-dir`, `local-dir`, `cache-dir`, `repo-dir`, and OS flags (`os/mac`, `os/linux`, …). |
| `init-config.el` | The `rivenEmacs` defcustom group + all customization variables (workspace, proxy, org dir, LSP modes, treesit auto-install). `rivenEmacs-setup-proxy`, `rivenEmacs-get-org-directory`. |
| `init-default.el` | Global defaults: no backups/lockfiles, `y`/`n` short answers, UTF-8, `delete-selection-mode`, line/column numbers, `recentf-mode`, `global-auto-revert-mode`, relocates ~30 builtin state files into `local/`. |
| `init-helper.el` | Utility commands: `get-project-root`, `consult-ripgrep-at-point/-region/-ex`, `riven/xref-find-definitions-or-search`, `+consult-fd-in-home`, `+open-in-system-explorer`, `riven/google-search`, `riven/google-translate`. |
| `init-use-package.el` | use-package / package-management bootstrap; requires `plz` (HTTP client). |

## `lisp/ui/` — User Interface

| File | Purpose |
|------|---------|
| `init-theme.el` | `ef-themes` (light `ef-deuteranopia-light` default via `rivenEmacs-light-theme`/`rivenEmacs-default-background`; `<f5>` toggles to the dark `ef-night` from `rivenEmacs-dark-theme`), `doom-modeline`, `nerd-icons`, `rainbow-delimiters`, `ligature` (Cascadia Code), `spacious-padding`, `fancy-compilation`, header-line file path, face overrides. |
| `init-font.el` | `riven/apply-fonts` — picks first available fixed / variable / CJK / emoji font per-platform. |
| `init-which-key.el` | `which-key` with minibuffer display, 1.0 s idle, `→` separator. |
| `init-hydra.el` | `hydra` (used by smerge). |

## `lisp/completion/` — Minibuffer & In-Buffer Completion

| File | Purpose |
|------|---------|
| `init-minibuffer.el` | `vertico` (+ directory/multiform/indexed), `orderless`, `marginalia`, `savehist`, `prescient`. The actual minibuffer stack. |
| `init-vertico.el` | Compatibility shim → loads `init-minibuffer`. |
| `init-consult.el` | `consult` (M-g/M-s/C-x bindings, preview-on-`M-.`), `embark` (`C-.` act, `C-c ;` dwim, `C-h B` bindings), `embark-consult`. |
| `init-completion-ui.el` | In-buffer completion: `corfu` (auto, cycle, `M-0..M-9` indexed insert), `corfu-popupinfo` (`M-h` toggles docs; `M-n`/`M-p` scroll the docs popup, falling back to next/previous candidate), `cape` (file/dabbrev/keyword), `nerd-icons-corfu`. Special eglot-managed capf ordering. |

## `lisp/editor/` — Editing Experience

| File | Purpose |
|------|---------|
| `init-editor.el` | `expand-region` (`C-=`), `iedit` (`C-;`), `avy`, `link-hint`, `sudo-edit`, `visual-regexp`, `color-rg`, `deadgrep`, `symbol-overlay` (`M-s o`), `hl-todo`, `goggles`, `jinx` spell-check (`M-$`). |
| `init-format.el` | `apheleia` (`C-S-i` → `riven/apheleia-format-dwim`), `aggressive-indent-mode` (size-guarded). |
| `init-pair.el` | `smartparens` for prog/text/markdown; auto-newline after `{` RET. |
| `init-fold.el` | `treesit-fold` (`M-i` toggle, falls back to `hs-minor-mode`), `hideif` for C/C++. |
| `init-crux.el` | `crux` (editing/buffer conveniences). |
| `init-markdown.el` | `markdown-ts-appear` rendered Markdown on the built-in `markdown-ts-mode`; `markdown-mode` remains the fallback (conceal, scaled headers). `grip-mode` browser preview (`C-c l p p`), rich `C-c l` keymap shared by both modes. |
| `init-autosave.el` | `super-save` (idle save, save on switch-window). |
| `init-undo.el` | `vundo` (`M-_`), `undo-fu` (undo/redo), `undo-fu-session` (persisted, compressed). |
| `init-editorconfig.el` | builtin `editorconfig-mode`. |

## `lisp/files/` — File Operations

| File | Purpose |
|------|---------|
| `init-dired.el` | `dirvish` (overrides dired, `C-x d`), vscode-icon, GLS detection, peek mode, `?` dispatch, `TAB` subtree, `M-j` fd-jump. |

## `lisp/parser/` — Tree-sitter

| File | Purpose |
|------|---------|
| `init-treesit.el` | 31 grammar sources; Emacs 31 native remap via `treesit-enabled-modes`. Commands: `install-treesit-language-grammars` / `auto-install-treesit-grammars` / `install-essential-treesit-grammars`. |

## `lisp/ide/` — IDE Features

| File | Purpose |
|------|---------|
| `init-eglot.el` | **The only LSP client** — builtin `eglot` + `eglot-booster`. On-demand server install, workspace defaults, HTML/web/JSON/YAML and correct LSP language ids, mouse-hover documentation (`riven/eglot-mouse-hover-mode`), signature-overload cycling (`C-c C-n`/`C-c C-p`), `riven/eglot-diagnose` (see [Language-Support](Language-Support.md)). `yasnippet`. Xref: `M-.`/`M-,`/`M-?`. |
| `init-checker.el` | `flymake` (debounced current-line echo in minibuffer, colored mode-line counters), `flymake-eslint` (prefers `oxlint`), `eslint-fix`. |
| `init-debugger.el` | `dape` (DAP client) under `C-c D`; saves/loads breakpoints, pulses source line. |
| `init-jump.el` | `switch-window` (`C-x o/1/2/3/0`), `goto-last-change` (`M-[`), `dumb-jump`. |
| `init-envrc.el` | `envrc` (direnv) when `direnv` binary present. |
| `init-project.el` | Minimal; aliases `project-switch-project-ex`. |

## `lisp/vcs/` — Version Control

| File | Purpose |
|------|---------|
| `init-vc.el` | `magit` (fullcolumn, gravatars, todos), `magit-todos`, `diff-hl` (gutter), `git-timemachine`, `git-modes`, `ediff` (horizontal), `smerge-mode` with auto-enabled conflicted buffers and `hydra/smerge` (`C-c m`). `C-c g m/P/r/A` → merge / preview / rebase / abort. `C-c h` → diff-hl-show-hunk. |

## `lisp/ai/` — AI Integration

See **[AI-Integration](AI-Integration.md)** for details.

| File | Purpose |
|------|---------|
| `init-gpt.el` | `gptel` (Zayfen / DeepSeek / OpenAI backends) + MCP tool servers. |
| `init-gpt-helper.el` | Fallback commands + review workflow (`riven/gptel-review-mode`). |
| `init-gpt-tools.el` | gptel core agent tools + `work` preset (`C-c a W`). |
| `init-ai-code.el` | `ai-code` CLI agents (Codex/Claude Code/Opencode/Aider), launched from the AI hub with `C-c a 1`..`5` or the `C-c a a` menu. |
| `init-dsh-emacs.el` | `dsh-emacs` — Emacs client for DeepSeek Harness (`C-c a d`): session list, streaming chat, subagents; lazy-loaded, manages its own `dsh web` server. |

## `lisp/lang/` — Per-Language Config

See **[Language-Support](Language-Support.md)**.

| File | Languages |
|------|-----------|
| `init-web.el` | HTML, CSS, JS/TS, Vue, Tailwind, emmet, React/JSX tree-sitter helpers. |
| `init-rust.el` | Rust (`rust-ts-mode`, cargo). |
| `init-python.el` | Python (Ruff flymake, pyvenv, pet). |
| `init-java.el` | Kotlin (`kotlin-ts-mode`); Java via builtin + eglot jdtls. |
| `init-swift.el` | Swift (`swift-mode`, sourcekit-lsp locator). |
| `init-org.el` | Org authoring, agenda, capture, babel, transclusion. Lazy via `org-mode-hook`. |

## `lisp/keymaps/` — Declarative Keybindings

See **[Architecture §Declarative Keybinding Engine](Architecture.md#declarative-keybinding-engine)** and **[Keybindings](Keybindings.md)**.

| File | Purpose |
|------|---------|
| `keybindings-engine.el` | Applies specs, clears owned prefixes, warns on duplicates/missing. |
| `keybindings-spec.el` | Aggregates all spec files. |
| `keybindings-spec-core.el` | File/Project/Git/Code/Search/Window/Note/Error specs. |
| `keybindings-spec-ai.el` | AI hub specs for `C-c a`: gptel chat/text actions, `ai-code` agents, `dsh-emacs`, MCP. |
| `keybindings-spec-lsp.el` | LSP specs. |
| `keybindings-spec-org.el` | Org/session specs. |
| `keybindings-commands.el` | Interactive helpers backing specs. |
| `init-keybindings.el` | Entry point; applies all specs from `after-init-hook` and configures which-key groups. |

## `lisp/session/` — Session Persistence

| File | Purpose |
|------|---------|
| `init-session.el` | `desktop.el`-based session persistence in `~/.cache/emacs/`. Commands `rivenEmacs-session-save/load/reload/clear/delete/current`. Autosave timer, lazy restore, lock-aware secondary instances. |

## `lisp/tools/` — Miscellaneous Tools

| File | Purpose |
|------|---------|
| `init-terminal.el` | `ghostel` is the primary terminal (`C-c t t`); `eat` and `vterm` are fallbacks. Selects a terminal-metric CJK font (Sarasa Term SC Nerd) for Ghostel and disables its CJK glyph shrinking. `eat` starts in line mode via `eat-exec-hook`. |
| `init-docker.el` | `dockerfile-mode`, `docker`. |
| `init-quickrun.el` | `quickrun`. |
| `init-feed.el` | `elfeed` RSS (~13 preconfigured feeds) + `elfeed-goodies`. |
| `init-lookup.el` | `devdocs` (per-language doc sets), `fanyi` dictionary. |
| `init-reader.el` | `reader` (epub/book reader from codeberg). |
| `init-codeforces.el` | Loads bundled `emacs-codeforces/` library. Commands via `M-x codeforces-*` (no default keybindings). |

## `lisp/writing/` — Writing & Export

| File | Purpose |
|------|---------|
| `ews.el` | Emacs Writing Studio: BibTeX, Denote PARA, distraction-free writing, org notes helpers. |
| `ox-beamer-lecture.el` | Custom Org Beamer back-end exporting lecture series to PDF. |

---

## Bundled Custom Code (non-ELPA)

- **`emacs-codeforces/`** — complete Codeforces competitive-programming client
  (api/auth/client/scrape/workspace/list/detail/agent + main). Cookie-session
  login, tag-filtered browsing, Org-rendered problem statements, rust solution
  scaffolding, real-time verdict polling.
- **`debug-adapters/`** — git submodule providing DAP debug adapter definitions.
- **`lisp/writing/`** — `ews.el` and `ox-beamer-lecture.el` (bundled, not from ELPA).

> **Note:** There is no `site-lisp/` directory. Bundled custom Elisp lives in
> `emacs-codeforces/` and `lisp/writing/`, plus the `debug-adapters/` submodule.
