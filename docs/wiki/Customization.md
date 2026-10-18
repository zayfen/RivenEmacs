# Customization

RivenEmacs exposes configuration through a single defcustom group plus a set of
environment variables. All defcustoms live under group **`rivenEmacs`**.

```elisp
M-x customize-group RET rivenEmacs RET
```

---

## Core Variables (`lisp/core/init-config.el`)

| Variable | Type | Default | Purpose |
|----------|------|---------|---------|
| `rivenEmacs-default-workspace` | directory | `~/` | Default project directory. |
| `rivenEmacs-proxy-http` | string | `127.0.0.1:7890` | HTTP proxy address. |
| `rivenEmacs-proxy-https` | string | `127.0.0.1:7890` | HTTPS proxy address. |
| `rivenEmacs-use-proxy` | boolean | `nil` | Master toggle for proxy. |
| `rivenEmacs-snippets-dir` | directory | `<emacs-dir>/snippets` | yasnippet snippets location. |
| `rivenEmacs-org-directory` | directory | `~/org` | Org files directory. |
| `rivenEmacs-dashboard-delay` | number | `0.5` | Delay before opening dashboard. |
| `rivenEmacs-lsp-modes` | list of symbols | (swift, python, js/ts, vue, rust, c/c++, java, kotlin, php, css, html/web, json, yaml, go, lua, bash — both `-ts-mode` and base `-mode`) | Modes where LSP is auto-enabled. |
| `rivenEmacs-eglot-workspace-configuration` | plist | Default workspace settings sent to Pyright/Pylsp/TypeScript servers. |
| `rivenEmacs-auto-install-treesit` | boolean | `nil` | Auto-install tree-sitter grammars at startup. |

---

## Session (`lisp/session/init-session.el`)

| Variable | Default | Purpose |
|----------|---------|---------|
| `rivenEmacs-session-directory` | `~/.cache/emacs/` | Fixed directory where desktop files are saved. |
| `rivenEmacs-session-auto-save-interval` | number | Autosave interval (seconds). |
| `rivenEmacs-session-save-geometry` | boolean | Persist frame/window geometry. |
| `rivenEmacs-session-restore-eager` | boolean | Restore buffers eagerly on load. |

---

## IDE / LSP

| Variable | File | Default | Purpose |
|----------|------|---------|---------|
| `rivenEmacs-eglot-auto-install` | `init-eglot.el` | `t` | Offer to install missing LSP servers on connect. |
| `rivenEmacs-eglot-install-timeout` | `init-eglot.el` | `300` | Abort an on-demand server install after this many seconds. |
| `rivenEmacs-eglot-disable-file-watch` | `init-eglot.el` | `t` | Disable dynamic LSP file-watching globally; set nil to restore it. |
| `rivenEmacs-eglot-inlay-hint-modes` | `init-eglot.el` | `nil` | Modes where Eglot inlay hints are auto-enabled (opt-in). |
| `rivenEmacs-eglot-workspace-configuration` | `init-eglot.el` | plist | Default workspace settings (Pyright basic typing, Pylsp fuzzy completion, TS relative imports); override per project with dir-locals. |
| `riven/flymake-echo-idle-delay` | `init-checker.el` | number | Delay before echoing current-line diagnostic. |
| `riven/treesit-auto-create-modes` | `init-treesit.el` | — | Modes where tree-sitter parsers are created eagerly. |
| `riven/aggressive-indent-max-size` | `init-format.el` | — | Skip aggressive-indent above this buffer size. |

---

## AI — gptel / MCP (`lisp/ai/init-gpt.el`)

| Variable | Default | Purpose |
|----------|---------|---------|
| `rivenEmacs-gptel-backend` | `zayfen` | Active gptel backend: `zayfen` / `deepseek` / `default`. |
| `rivenEmacs-zayfen-host` | `api.zayfen.com` | Zayfen API host. |
| `rivenEmacs-zayfen-chat-models` | `(deepseek-v4-flash …)` | Models offered via `/v1/chat/completions`. |
| `rivenEmacs-zayfen-responses-models` | `(gpt-5.4 gpt-5.5)` | Models offered via `/v1/responses`. |
| `rivenEmacs-zayfen-default-model` | — | Default model. |
| `rivenEmacs-gptel-chatgpt-oauth-backend` | `nil` | Optional ChatGPT Plus/Pro Codex OAuth backend name. |
| `rivenEmacs-gptel-chatgpt-oauth-model` | `gpt-5.3-codex` | Model for Work mode when the OAuth backend is active. |
| `rivenEmacs-gptel-tool-roots` | `nil` | Directories the Work mode core tools may access (nil = project root/workspace). |
| `rivenEmacs-gptel-tool-max-file-bytes` | `200000` | Max file size for `read_file`. |
| `rivenEmacs-gptel-tool-max-output-chars` | `20000` | Max output returned by a core tool. |
| `rivenEmacs-gptel-mcp-auto-connect` | `nil` | Connect MCP servers on startup. |
| `rivenEmacs-mcp-filesystem-roots` | list | Roots the filesystem MCP server may access. |
| `rivenEmacs-mcp-enable-github-server` | — | Toggle GitHub MCP server. |
| `rivenEmacs-mcp-enable-browser-server` | — | Toggle Playwright browser MCP server. |
| `rivenEmacs-mcp-enable-browser-use-server` | — | Toggle browser-use MCP server. |
| `rivenEmacs-mcp-enable-brave-search-server` | — | Toggle Brave search MCP server. |
| `rivenEmacs-mcp-enable-tavily-server` | — | Toggle Tavily search MCP server. |
| `rivenEmacs-mcp-brave-api-key-env` | — | Env var name for Brave key. |
| `rivenEmacs-mcp-tavily-api-key-env` | — | Env var name for Tavily key. |
| `rivenEmacs-mcp-browser-headless` | — | Run browser MCP headless. |
| `rivenEmacs-mcp-memory-file` | — | Path to MCP memory store. |
| `rivenEmacs-gptel-shell-env-fallback` | — | Use login shell to read API keys. |

**AI — ai-code** (`init-ai-code.el`): `rivenEmacs-ai-code-backend` —
`codex` (default) / `claude-code` / `opencode` / `aider`.

**Review workflow** (`init-gpt-helper.el`): `rivenEmacs-gptel-max-input-chars`.

---

## Other Groups

| Group | File | Purpose |
|-------|------|---------|
| `codeforces` | `emacs-codeforces/` | Codeforces client config. |
| `ews` | `lisp/writing/ews.el` | Emacs Writing Studio. |
| `org-export-beamer-lecture` | `lisp/writing/ox-beamer-lecture.el` | Beamer lecture export. |

---

## Environment Variables

Imported by `exec-path-from-shell` 2 s after init (configured in `init.el`).

### Proxy / workspace
- `DEFAULT_WORKSPACE` — default project directory.
- `HTTP_PROXY` / `HTTPS_PROXY` — network proxy.

### AI service keys
- `ZAYFEN_API_KEY`, `DEEPSEEK_API_KEY`, `GROQ_API_KEY`, `OPENAI_API_KEY`
- `ANTHROPIC_AUTH_TOKEN`, `ANTHROPIC_BASE_URL`
- `BRAVE_API_KEY`, `TAVILY_API_KEY` — for MCP search servers (absence
  auto-skips those servers, non-blocking)
- `GITHUB_TOKEN`, `GITHUB_PERSONAL_ACCESS_TOKEN`

### System / shell passthrough
- `SSH_AUTH_SOCK`, `SSH_AGENT_PID`, `GPG_AGENT_INFO`
- `LANG`, `LC_CTYPE`
- `NIX_SSL_CERT_FILE`, `NIX_PATH`

### Writing them

```bash
bash scripts/riven-deps.sh doctor    # report what's missing
bash scripts/riven-deps.sh fix       # interactively write to shell profile
```

`riven-deps.sh` writes all of the above **except** `ZAYFEN_API_KEY`.

---

## Recommended Setup

For a personal `early-init.el`-free customization, drop overrides into a file
on `load-path` or use the `custom-file` (`custom.el`). Example:

```elisp
;; ~/.emacs.d/lisp/local/my-config.el
(with-eval-after-load 'init-config
  (setq rivenEmacs-default-workspace "~/Code"
        rivenEmacs-use-proxy t
        rivenEmacs-eglot-auto-install t))
```
