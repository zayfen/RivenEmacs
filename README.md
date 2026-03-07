# RivenEmacs

A highly modular and performance-optimized Emacs configuration designed for modern development workflows.

## Quick Start

```bash
# 1) Check current machine health (deps + env + batch init)
bash scripts/riven-deps.sh doctor

# 2) Auto-fix missing deps and prompt for missing env vars
bash scripts/riven-deps.sh fix

# 3) Verify again
bash scripts/riven-deps.sh doctor
```

## Dependency Script

RivenEmacs ships a dependency management script:

```bash
bash scripts/riven-deps.sh --help
```

### Commands

- `install`: Install dependency groups.
- `doctor`: Check dependencies, environment variables, and `emacs --batch -l init.el` runtime loading.
- `fix`: Install missing dependencies and interactively prompt for missing environment variables.
- `list`: Print dependency groups and tracked env vars.

### Install Examples

```bash
# Default install groups: core + lsp
bash scripts/riven-deps.sh install

# Install all groups
bash scripts/riven-deps.sh install --all

# Install AI + MCP stack only
bash scripts/riven-deps.sh install --ai --mcp

# Install spell-check runtime for jinx
bash scripts/riven-deps.sh install --spell
```

### Fix Examples

```bash
# Default fix groups: core + lsp + ai + mcp + markdown + spell
bash scripts/riven-deps.sh fix

# Only prompt and write env vars (no package install)
bash scripts/riven-deps.sh fix --env-only

# Only install deps (no env prompt)
bash scripts/riven-deps.sh fix --deps-only

# Write env vars to a specific profile file
bash scripts/riven-deps.sh fix --profile ~/.zshrc
```

## Dependency Groups

- `core`: `emacs`, `git`, `ripgrep`, `fd`, `node`, `npm`, `python3`
- `lsp`: `ruff`, `pyright`, `typescript-language-server`, `vtsls`, `vue-language-server`, `emacs-lsp-booster`, etc.
- `ai`: `cursor-agent-acp`, `claude`, `opencode`
- `mcp`: filesystem/memory/sequential-thinking/everything/playwright/browser-use/brave/tavily MCP tools
- `markdown`: `go-grip`
- `spell`: `enchant` + `hunspell` (required by `jinx`)
- `extra`: `docker` and extra quality-of-life tooling

## LSP & Completion Stack (方案1)

- LSP client: `eglot` (enabled for modes in `rivenEmacs-lsp-modes`)
- Completion UI: `vertico + consult + orderless + corfu + cape`
- Performance booster: `emacs-lsp-booster` (auto-enabled when executable exists in `PATH`)
- ElDoc behavior: single-line minibuffer display with a short idle delay (`eldoc-idle-delay 0.15`)
- Cursor-line stability: Eglot code-action hints are shown in mode-line (not margin/nearby)
- `lsp-bridge` files are kept in repo, but no longer loaded by default in `init.el`

## Modern Plugin Stack

- Formatting: `apheleia` (manual format key: `C-S-i`, region or whole buffer)
- Completion actions: `embark` + `embark-consult` (`C-.`, `C-;`, `C-h B`)
- Code completion: `corfu + cape` (with `orderless`, wider popup for coding buffers)
- Candidate docs: `corfu-popupinfo` (`M-h` to open candidate documentation popup)
- Tree-sitter UX: `treesit-auto` (prompted grammar install + modern remapping)
- Spell check: `jinx` (`M-$` correct, `C-M-$` set languages)

## Diagnostics & Documentation UX

- `C-c c e`: open buffer diagnostics and focus the Flymake diagnostics window
- After `C-c c e`, press `C-g` in the source code buffer to close that diagnostics window quickly
- `C-c c k`: open ElDoc documentation and focus the ElDoc documentation window
- Flymake keeps underlines in buffer and echoes current-line diagnostics in minibuffer

## Environment Variables

The script tracks and can prompt to set these variables:

- `DEFAULT_WORKSPACE`
- `DEEPSEEK_API_KEY`
- `GROQ_API_KEY`
- `ANTHROPIC_AUTH_TOKEN`
- `ANTHROPIC_BASE_URL`
- `OPENAI_API_KEY`
- `BRAVE_API_KEY`
- `TAVILY_API_KEY`
- `HTTP_PROXY` / `HTTPS_PROXY`

## AI & MCP Notes

- GPT/MCP configuration lives in [lisp/ai/init-gpt.el](lisp/ai/init-gpt.el).
- MCP servers can be verified in Emacs with:

```elisp
M-x riven/gptel-mcp-verify
```

If `BRAVE_API_KEY` or `TAVILY_API_KEY` is missing, corresponding MCP servers are skipped automatically.

## Manual Build/Lint/Test Commands

```bash
# Check configuration syntax (all files)
emacs --batch -l init.el --eval "(message \"Config loaded successfully\")"

# Byte compile a single file
emacs --batch -f batch-byte-compile lisp/init-config.el

# Byte compile all configuration files
emacs --batch -f batch-byte-compile lisp/*.el

# Load init.el and test basic functionality
emacs --batch -l init.el --eval "(message \"All modules loaded\")"
```

## Tree-sitter Configuration

RivenEmacs includes `treesit-auto` for automatic mode remapping and prompted grammar installation.

### Manual Installation

```elisp
M-x auto-install-treesit-grammars
M-x install-essential-treesit-grammars
```

### Troubleshooting

1. Ensure network access for grammar sources.
2. Ensure build tools are installed (`git`, `gcc`, `make`, etc.).
3. Prefer `M-x install-essential-treesit-grammars` for a smaller baseline.
