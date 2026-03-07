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
```

### Fix Examples

```bash
# Default fix groups: core + lsp + ai + mcp + markdown
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
- `lsp`: `ruff`, `pyright`, `typescript-language-server`, `vtsls`, `vue-language-server`, etc.
- `ai`: `cursor-agent-acp`, `claude`, `opencode`
- `mcp`: filesystem/memory/sequential-thinking/everything/playwright/browser-use/brave/tavily MCP tools
- `markdown`: `go-grip`
- `extra`: `docker` and extra quality-of-life tooling

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

RivenEmacs includes tree-sitter support for enhanced syntax highlighting. By default, tree-sitter grammars are not automatically installed to avoid startup delays and network issues.

### Manual Installation

```elisp
M-x auto-install-treesit-grammars
M-x install-essential-treesit-grammars
```

### Troubleshooting

1. Ensure network access for grammar sources.
2. Ensure build tools are installed (`git`, `gcc`, `make`, etc.).
3. Prefer `M-x install-essential-treesit-grammars` for a smaller baseline.
