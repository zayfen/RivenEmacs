# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**RivenEmacs** is a highly modular, performance-optimized Emacs configuration (target Emacs 31.1) for modern development workflows: declarative `C-c` keybindings, tree-sitter, Eglot-based LSP, AI/MCP integration, and comprehensive language support. If this file and `AGENTS.md` disagree, `AGENTS.md` plus the actual code wins.

## Architecture

### Core Structure
- **early-init.el**: startup performance (GC, file-name handlers, native-comp cache), package archives, `custom-file`
- **init.el**: assembly only — `load-path` construction, package system, phased `require`s
- **lisp/**: feature modules grouped by concern (see `docs/wiki/Modules.md`)
- **local/**: user-specific data (gitignored: cache, savehist, eln; desktop sessions now live in `~/.cache/emacs/`)
- **elpa/**: package installation directory

### Key Modules (paths are relative to `lisp/`)
- **core/init-config.el**: `rivenEmacs` defcustom group (workspace, proxy, LSP modes, treesit)
- **core/init-default.el**: global defaults and state-file relocation into `local/`
- **core/init-helper.el**: shared commands (`riven/*`, `+consult-*`, `riven/xref-find-definitions-or-search`, …)
- **ide/init-eglot.el**: **the only LSP client** — Eglot + `eglot-booster`, on-demand server install
- **ai/init-gpt.el** / **ai/init-ai-code.el** / **ai/init-dsh-emacs.el**: gptel + MCP; CLI coding agents; DeepSeek Harness client (`C-c d`)
- **keymaps/**: declarative keybinding spec files, engine, and helper commands
- **tools/init-terminal.el**: terminal — `ghostel` primary, `eat`/`vterm` fallbacks
- **vcs/init-vc.el**: magit, diff-hl, git-timemachine, ediff/smerge

`lsp-bridge` has been removed — there is no `init-lsp-bridge.el` and no
`lsp-bridge-*` commands. `general.el`/`SPC` leader are also retired.

## Development Commands

```bash
# Run the offline test suites (project ERT + emacs-codeforces ERT + Python unit tests)
scripts/run-tests.sh

# Run a single ERT file
scripts/run-tests.sh tests/test-init-terminal.el
scripts/run-tests.sh emacs-codeforces/test/emacs-codeforces-auth-test.el

# Python agent unit tests only
python3 -m unittest discover -s emacs-codeforces/test -v

# Byte compile one module
emacs --batch -f batch-byte-compile lisp/core/init-config.el

# Interactive smoke test (may install packages and use the network)
emacs -q -l /path/to/RivenEmacs/init.el
```

The startup-integration tests that spawn a nested `emacs -l init.el` are
opt-in: `RIVENEMACS_RUN_INIT_TESTS=1 scripts/run-tests.sh`.

### Package Management
- **Install packages**: automatic via use-package on startup
- **Update packages**: `M-x package-list-packages` then `U x`
- **Clean packages**: `M-x package-autoremove`

### Tree-sitter Grammar Management
```elisp
;; Install tree-sitter grammars
M-x install-essential-treesit-grammars  ;; Core languages
M-x auto-install-treesit-grammars       ;; All supported grammars

;; Configure auto-installation
(setq rivenEmacs-auto-install-treesit t)
```

### LSP Configuration
- **Language servers**: configured via Eglot in `lisp/ide/init-eglot.el`
- **Python**: server priority chain basedpyright → pyright → pylsp → ruff
- **JS/TS**: `typescript-language-server` / `vtsls`; Vue via Volar
- **Rust**: rust-analyzer (on-demand install)
- **Auto-install**: `rivenEmacs-eglot-auto-install` (default `t`)

## Environment Configuration

### Environment Variables
- `DEFAULT_WORKSPACE`: Default project directory (default: `~/`)
- `GROQ_API_KEY`, `DEEPSEEK_API_KEY`, `OPENAI_API_KEY`: AI services
- `ANTHROPIC_AUTH_TOKEN`, `ANTHROPIC_BASE_URL`: Claude/Anthropic integrations
- `BRAVE_API_KEY`, `TAVILY_API_KEY`, `GITHUB_TOKEN`: MCP tool servers
- `HTTP_PROXY`/`HTTPS_PROXY`: Network proxy configuration
- `CODEFORCES_HOME`: optional home override for the Codeforces client

### Proxy Setup
```elisp
;; Enable proxy
(setq rivenEmacs-use-proxy t)
;; Or use environment variables
(setenv "HTTP_PROXY" "127.0.0.1:7890")
```

## Key Bindings

### Leader key: `C-c` (declarative engine)
All leader bindings live in spec files under `lisp/keymaps/`
(`keybindings-spec-core.el`, `-ai.el`, `-lsp.el`, `-org.el`) and are applied by
`keybindings-engine.el` on `after-init-hook`. Do **not** use `general.el` or
`leader-def`; add a spec triple `(key command "which-key label")` instead.

- **C-c f**: File operations
- **C-c p**: Project
- **C-c s**: Search
- **C-c g**: Git
- **C-c c**: Code actions (Eglot/xref; global spec, no eglot-mode-map copy)
- **C-c e**: Errors / Flymake
- **C-c t**: Tools (terminal `C-c t t`, docker, quickrun, feed, reader, …)
- **C-c w**: Windows
- **C-c a**: AI (gptel)
- **C-c =**: AI coding agents (ai-code)
- **C-c d**: DeepSeek Harness client (dsh-emacs; direct launcher, not a group)
- **C-c D**: dape debugger
- **C-c n**: Org notes
- **C-c x**: Session

### Global Bindings
- `M-.` / `M-,` / `M-?`: Eglot/xref definition / back / references
- `M-*`: `ai-code-menu`
- `M-g`: navigation prefix (avy, imenu, mark, next/prev error, diff-hl hunks)
- `C-c h`: `diff-hl-show-hunk`; `C-c ;`: `embark-dwim`
- `M-y`: `consult-yank-pop`; `C-x b`: `consult-buffer`
- `C-.`: `embark-act`
- `C-c t t`: terminal (`ghostel`); see `docs/wiki/Keybindings.md` for the
  authoritative list

## Language Support

### Web Development
- HTML, CSS, JavaScript, TypeScript
- Vue, React, Tailwind CSS
- ESLint integration

### Systems Programming
- Rust (rust-analyzer)
- C/C++ (clangd)
- Python (basedpyright/pyright/ruff)

### Configuration Files
- JSON, YAML, TOML
- Docker, docker-compose
- EditorConfig

## Customization

### Adding New Languages
1. Install the language server binary
2. Add the mode to `rivenEmacs-lsp-modes` in `lisp/core/init-config.el`
3. Register the server/install command in `lisp/ide/init-eglot.el`
   (see `riven/eglot-register-simple-server`)

### Adding Keybindings
- Add a spec entry under the right group in `lisp/keymaps/keybindings-spec-*.el`
- Backing commands go in `lisp/keymaps/keybindings-commands.el`
- Global non-leader keys use `keymap-global-set`
- Add or extend ERT coverage; run `scripts/run-tests.sh`

### Custom Variables
- Use `M-x customize-group RET rivenEmacs RET`
- Or set directly: `(setq rivenEmacs-default-workspace "~/projects/")`

## Performance Tuning

### Startup Optimization
- Lazy loading via use-package
- Heavy modules deferred to `after-init-hook` at depth -20 (during startup,
  before desktop restore/keybinding handlers; still synchronous, not
  idle-deferred)
- Native compilation cache in `local/eln/`

### Memory Management
- Aggressive garbage collection during startup
- Restored to normal after initialization
- Configurable via `early-init.el`

## Troubleshooting

### Common Issues
1. **Tree-sitter errors**: Check network connectivity and git authentication, then run `M-x install-essential-treesit-grammars`
2. **LSP not starting**: Verify the language server binary is on `PATH` (`M-x eglot-reconnect`; see `docs/wiki/Language-Support.md`)
3. **Slow startup**: Check package conflicts; re-run `emacs -Q --batch -l benchmark-startup.el`
4. **Proxy issues**: Configure `rivenEmacs-use-proxy` and environment variables

### Debug Commands
```elisp
;; Check configuration
M-x describe-variable RET rivenEmacs-auto-install-treesit RET

;; Check Eglot status
M-x eglot-events-buffer
M-x eglot-reconnect

;; Check package status
M-x list-packages
```

## Testing

```bash
# Everything (offline): project ERT + emacs-codeforces ERT + Python unit tests
scripts/run-tests.sh

# A single ERT file
scripts/run-tests.sh tests/test-init-terminal.el
```

The runner builds `load-path` from the repository, prefers checked-in sources
over stale `.elc`, never loads `init.el`, and returns non-zero on failure.
See `docs/wiki/Development.md` for the test layout.
