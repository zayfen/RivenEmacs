# AI Integration

RivenEmacs treats AI as a first-class citizen. There are four independent
layers, all under `lisp/ai/`:

1. **gptel** — chat-style LLM client (Zayfen / DeepSeek / OpenAI backends)
2. **MCP** — Model Context Protocol tool servers (filesystem, browser, search, …)
3. **ai-code** — CLI coding agents (Codex / Claude Code / Opencode / Aider)
4. **dsh-emacs** — Emacs client for DeepSeek Harness (`C-c a d`)

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
- `C-c a W` → `riven/gptel-work` — open a Work-style agent chat with core tools.
- `C-c a R` → `gptel-extensions-refactor` — refactor region/buffer.
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

## 2. ChatGPT Work-style mode — `C-c a W`

ChatGPT Work is a ChatGPT product mode without a public API. RivenEmacs
provides a local equivalent for gptel: the `work` preset defined in
`lisp/ai/init-gpt-tools.el`.

- `M-x riven/gptel-work` (also `C-c a W`) opens a dedicated Work buffer and
  applies the preset buffer-locally.
- `@work ...` in any gptel prompt applies the same preset to that one query.

The preset activates seven core tools and asks for confirmation before
destructive actions:

| Tool | Purpose |
|------|---------|
| `read_file` | read a text file under the workspace root |
| `list_directory` | list a directory |
| `grep_files` | search with ripgrep |
| `write_file` | create/overwrite a file (confirmation required) |
| `run_shell_command` | run a shell command in the root (confirmation required) |
| `web_search` | Tavily search (requires `TAVILY_API_KEY`) |
| `read_url` | fetch and extract readable page text |

Safety defaults: every path must resolve below the current project root (or
`rivenEmacs-default-workspace`); use `rivenEmacs-gptel-tool-roots` to narrow
or extend that root. Tool results are shown as calls only so long file or
search output does not flood the chat.

If you have a ChatGPT Plus/Pro subscription, gptel's Codex OAuth backend can
be used instead of an API key: set `rivenEmacs-gptel-chatgpt-oauth-backend`
(for example to `"ChatGPT-Codex"`), run `M-x gptel-openai-oauth-login` once,
and `C-c a W` will prefer that backend when it is registered.  Its model is
`rivenEmacs-gptel-chatgpt-oauth-model` (default `gpt-5.3-codex`).

---

## 3. MCP — Tool Servers

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

- `M-x riven/gptel-mcp-verify` (also `C-c a M`) — connect all enabled servers
  and validate the tool registration in one step; reports which servers
  connected and which were skipped.
- `M-x riven/gptel-mcp-connect-popular` — the underlying connect-only command.

`rivenEmacs-gptel-mcp-auto-connect` defaults to `nil`; set it to `t` to
connect configured MCP servers automatically at startup, or use `C-c a M`
for one-off connections.

---

## 4. ai-code — CLI Coding Agents

Configured in `lisp/ai/init-ai-code.el` via `tninja/ai-code-interface.el`.
Launches a coding agent in a terminal pane (backend: `ghostel`).

| `rivenEmacs-ai-code-backend` | Agent | Notes |
|------------------------------|-------|-------|
| **`codex`** (default) | OpenAI Codex CLI | |
| `claude-code` | Anthropic Claude Code | `ANTHROPIC_AUTH_TOKEN` |
| `opencode` | opencode | |
| `aider` | aider | |

Two-column `ai-code-menu` integrates with magit transients for staging context.

**Entry points** (all under the `C-c a` AI hub):

- `C-c a a` → `ai-code-menu` (the single entry point for the menu)
- `C-c a 1` / `2` / `3` / `4` / `5` → codex / opencode / cursor-cli / claude-code / aider directly

Switch backend on the fly: `M-x riven/ai-code-configure-backend`.

---

## 5. dsh-emacs — DeepSeek Harness client

Configured in `lisp/ai/init-dsh-emacs.el` via
[`vritser/dsh-emacs`](https://github.com/vritser/dsh-emacs). It talks to a
DeepSeek Harness (`dsh web`) server over HTTP/WebSocket and offers a session
list grouped by workspace, streaming chat with foldable tool calls and
reasoning, model/effort selection, attachments, and subagents.

- `C-c a d` → `dsh-emacs` (session list); the package and server load lazily on
  the first press.
- With the default `dsh-emacs-base-url` the client manages a loopback server:
  it starts `dsh web --host 127.0.0.1 --port 3080 --no-open`, captures the
  launch token, and stops that server when Emacs exits.
- To attach to a server started outside Emacs (for example the browser UI's
  `dsh web`), set `dsh-emacs-base-url` to its printed URL including the
  `?token=...` query and set `dsh-emacs-server-auto-start` to `nil`.
- `M-x dsh-emacs-open-web` opens the server's web UI for account sign-in;
  `M-x dsh-emacs-add-provider` configures providers and models.
- Requires the `dsh` CLI (0.2.1-rc.2 or newer) on `exec-path`; `dsh-emacs`
  offers to install it when missing.

The debugger's dape prefix is `C-c D`.  The old direct `C-c d` launcher is
retired now that the DSH client lives under the `C-c a` hub.

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

> **Note:** GitHub Copilot is **not** configured. In-Emacs AI is gptel +
> ai-code + dsh-emacs.
