# AI Integration

RivenEmacs treats AI as a first-class citizen. There are three independent
layers, all under `lisp/ai/`:

1. **gptel** — chat-style LLM client (Zayfen / DeepSeek / OpenAI backends)
2. **MCP** — Model Context Protocol tool servers (filesystem, browser, search, …)
3. **ai-code** — CLI coding agents (Codex / Claude Code / Opencode / Aider)

Each layer degrades gracefully when its API key / binary is missing.

---

## 1. gptel — Chat LLM Client

Configured in `lisp/ai/init-gpt.el`. The active backend is chosen by
`rivenEmacs-gptel-backend`:

| Backend | API host | Key variable | Models |
|---------|----------|--------------|--------|
| **`zayfen`** (default) | `api.zayfen.com` | `ZAYFEN_API_KEY` | chat: `deepseek-v4-flash`, `deepseek-v4-pro`, `glm-5`, `glm-5-turbo`; responses: `gpt-5.4`, `gpt-5.5` |
| `deepseek` | DeepSeek | `DEEPSEEK_API_KEY` | via `gptel-openai-extras` |
| `default` | OpenAI | `OPENAI_API_KEY` | OpenAI models |

API keys are read from the environment with a **login-shell fallback** and an
**`auth-source` fallback**, so you can store them in `~/.authinfo` instead of
exporting them.

**Key commands** (see also [Keybindings](Keybindings.md) → `C-c a`):

- `M-x gptel` / `gptel-menu` / `gptel-send` / `gptel-rewrite` — chat, menu, send, rewrite.
- `C-x =` → `gptel-extensions-refactor` — refactor region/buffer.
- Auto-scroll and end-of-response hooks are wired in.

### gptel-extensions + helper fallbacks

`init-gpt-helper.el` provides a **review-buffer workflow**: AI output appears in
a special `riven/gptel-review-mode` buffer where:

| Key | Action |
|-----|--------|
| `q` | close review |
| `y` | accept — insert below |
| `r` | accept — replace original |

Commands:

- `riven/gptel-translate-region-review`, `riven/gptel-summarize-region-review`
- Fallbacks: `gptel-translate-region`, `gptel-extensions-ask-document`,
  `gptel-rewrite-article`, `gptel-summarize-document`,
  `gptel-query-devdoc`, `gptel-generate-commit-message`

These fallbacks keep the `C-c a` keybindings working even when optional
`gptel-extensions` is not installed.

---

## 2. MCP — Tool Servers

RivenEmacs configures MCP (Model Context Protocol) servers so gptel can call
external tools (read files, browse the web, run searches, remember things).
Via `lizqwerscott/mcp.el` + `gptel-integrations`, in `lisp/ai/init-gpt.el`.

Servers are declared in `riven/gptel-mcp-popular-servers`:

| Server | Purpose | Required |
|--------|---------|-----------|
| filesystem | file read/write within configured roots | always on |
| memory | persistent key-value memory | always on |
| sequential-thinking | step-by-step reasoning | always on |
| everything | desktop file search | always on |
| github | repo operations | `rivenEmacs-mcp-enable-github-server` + `GITHUB_TOKEN` |
| browser (Playwright) | headless browser automation | `rivenEmacs-mcp-enable-browser-server` |
| browser-use | alternative browser automation | `rivenEmacs-mcp-enable-browser-use-server` |
| brave-search | web search | `rivenEmacs-mcp-enable-brave-search-server` + `BRAVE_API_KEY` |
| tavily | search API | `rivenEmacs-mcp-enable-tavily-server` + `TAVILY_API_KEY` |

**Graceful degradation**: each server checks for its binary/API key at connect
time and silently skips itself if missing — no errors, no blocking.

**Commands**:

- `M-x riven/gptel-mcp-connect-popular` (also `C-c a M`) — connect all enabled servers.
- `M-x riven/gptel-mcp-verify` (also `C-c a V`) — validate MCP config; reports
  which servers connected and which were skipped.

`rivenEmacs-gptel-mcp-auto-connect` (default `t`) connects on startup.

---

## 3. ai-code — CLI Coding Agents

Configured in `lisp/ai/init-ai-code.el` via `tninja/ai-code-interface.el`.
Launches a coding agent in a terminal pane (backend: `ghostel`).

| `rivenEmacs-ai-code-backend` | Agent | Notes |
|------------------------------|-------|-------|
| **`codex`** (default) | OpenAI Codex CLI | |
| `claude-code` | Anthropic Claude Code | `ANTHROPIC_AUTH_TOKEN` |
| `opencode` | opencode | |
| `aider` | aider | |

Two-column `ai-code-menu` integrates with magit transients for staging context.

**Entry points**:

- `M-*` (global) → `ai-code-menu`
- `C-c = =` → `ai-code-menu`
- `C-c = 1` / `2` / `3` → claude-code / opencode / cursor-cli directly

Switch backend on the fly: `M-x riven/ai-code-configure-backend`.

---

## Prompt Templates

`lisp/ai/init-prompt-template.el` centralizes shared prompts as defcustoms:

- `riven-prompt-explain-code`
- `riven-prompt-refactor-code`
- `riven-prompt-add-comments`
- `riven-prompt-fix-errors`

Edit them via `M-x customize-group RET rivenEmacs RET`.

---

## Environment Variables Summary

| Variable | Used by |
|----------|---------|
| `ZAYFEN_API_KEY` | gptel Zayfen backend |
| `DEEPSEEK_API_KEY` | gptel DeepSeek backend |
| `OPENAI_API_KEY` | gptel OpenAI backend |
| `ANTHROPIC_AUTH_TOKEN`, `ANTHROPIC_BASE_URL` | claude-code agent |
| `GROQ_API_KEY` | (reserved) |
| `BRAVE_API_KEY` | MCP brave-search server |
| `TAVILY_API_KEY` | MCP tavily server |
| `GITHUB_TOKEN` / `GITHUB_PERSONAL_ACCESS_TOKEN` | MCP github server |

All imported by `exec-path-from-shell` 2 s after init. Use
`bash scripts/riven-deps.sh doctor` to check which are set.

> **Note:** GitHub Copilot is **not** configured. In-Emacs AI is gptel + ai-code.
