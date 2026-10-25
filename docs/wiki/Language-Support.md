# Language Support

RivenEmacs uses **eglot as the only LSP client** (with `eglot-booster`).
`lsp-bridge` was retired and its module removed; any `lsp-bridge-*` command in
older notes is historical and unavailable.

A signature feature: **on-demand server installation**. When you open a file and
the language server binary is not on `PATH`, eglot prompts in the minibuffer
with the exact install command; answering `y` installs it (npm/pip/brew/
composer/rustup) and connects.

---

## How Auto-Install Works

Defined in `lisp/ide/init-eglot.el`. At connect time, eglot calls a
**function-form contact** which:

1. First prefers a server that is already on `PATH`.  For Python the order is
   basedpyright → pyright → pylsp → ruff, so with Pyright installed no
   BasedPyright install prompt is shown.
2. Only when *none* of the candidate servers is installed, and
   `rivenEmacs-eglot-auto-install` is non-nil, prompts for the missing ones in
   preference order:
   ```
   Eglot: `pyright-langserver' not found. Install (npm i -g pyright)? (y or n)
   ```
   Declining or quitting one prompt (`C-g`) skips only that server and moves on
   to the next candidate instead of aborting the whole connection attempt.
3. On `y`, runs the installer as a subprocess while continuing to process
   input/redisplay, showing progress in the minibuffer and keeping the
   `*eglot server install*` output buffer.  The wait is bounded by
   `rivenEmacs-eglot-install-timeout` (default 300s).
4. Refreshes `exec-path` with common npm/pip/cargo/Brew binary directories,
   then returns the contact once the binary is available.
5. A declined install is remembered for the session; run
   `M-x riven/eglot-reset-install-prompts` to allow prompting again.

Disable globally: `(setq rivenEmacs-eglot-auto-install nil)`.

When a server appears to be connected but completion does not pop up, run
`M-x riven/eglot-diagnose`.  It reports whether Eglot actually manages the
buffer, its server process, the effective `completion-at-point-functions`, and
the Corfu auto-popup state, plus a targeted hint for each failure mode (for
example a Rust file without a Cargo project).

On-type formatting (`documentOnTypeFormattingProvider`) is disabled globally:
some servers issue synchronous requests for characters such as `;` and RET,
which can freeze Emacs. Use format-on-save or explicit format commands instead.

LSP dynamic file watching is disabled by default
(`rivenEmacs-eglot-disable-file-watch`), because some servers misbehave with
Eglot's `didChangeWatchedFiles` registration; set it to nil to restore the
standard behavior.

Inlay hints are opt-in through `rivenEmacs-eglot-inlay-hint-modes`, for
example `(setq rivenEmacs-eglot-inlay-hint-modes '(rust-ts-mode
typescript-ts-mode))`.  Reconnect Eglot after changing the list.

---

## Workspace Configuration

`rivenEmacs-eglot-workspace-configuration` (see `init-eglot.el`) supplies
default server settings through `eglot-workspace-configuration`:

- Pyright: `python.analysis.typeCheckingMode = "basic"` (a project
  `pyrightconfig.json` still wins);
- Pylsp: fuzzy Jedi completion;
- TypeScript/JavaScript: relative import paths.

Projects can override any of this with directory-local
`eglot-workspace-configuration`, which takes precedence over the defaults.
Inspect the effective payload with `M-x eglot-show-workspace-configuration`.

Server contacts also preserve standard LSP language ids: `web-mode` sends
`html`, `js-ts-mode` sends `javascript`, `tsx-ts-mode` sends
`typescriptreact`, and the JSON/YAML modes send `json`/`jsonc`/`yaml`.

---

## Supported Languages

| Language | Modes | Server (priority order) | Install command |
|----------|-------|------------------------|-----------------|
| **Python** | `python-mode`, `python-ts-mode` | basedpyright → pyright → pylsp → ruff | `pip install basedpyright` / `npm i -g pyright` / `pip install ruff` / `pip install python-lsp-server` |
| **Vue** | `vue-ts-mode` | vue-language-server (Volar) + TypeScript | `npm i -g @vue/language-server @vue/language-core` (+ `typescript`) |
| **Kotlin** | `kotlin-mode`, `kotlin-ts-mode` | kotlin-lsp (if present) → kotlin-language-server | `npm i -g kotlin-language-server` |
| **Rust** | `rust-mode`, `rust-ts-mode` | rust-analyzer | `rustup component add rust-analyzer` |
| **C / C++** | `c-mode`, `c-ts-mode`, `c++-mode`, `c++-ts-mode`, `objc-mode` | clangd | `brew install llvm` |
| **Java** | `java-mode`, `java-ts-mode` | jdtls | `brew install jdtls` |
| **JS / TS** | `typescript-ts-mode`, `tsx-ts-mode`, `js-ts-mode` | typescript-language-server | `npm i -g typescript-language-server typescript` |
| **JSX** | `js-ts-mode` (`.jsx` is mapped there) | typescript-language-server | same as JS/TS |
| **HTML** | `web-mode` (`.html`/`.ejs`), `html-mode`, `html-ts-mode` | vscode-html-language-server | `npm i -g vscode-langservers-extracted` |
| **JSON** | `js-json-mode`, `json-mode`, `json-ts-mode`, `jsonc-mode` | vscode-json-language-server | `npm i -g vscode-langservers-extracted` |
| **YAML** | `yaml-mode`, `yaml-ts-mode`, `yaml-ts-mode-maybe` | yaml-language-server | `npm i -g yaml-language-server` |
| **Go** | `go-mode`, `go-ts-mode` | gopls | `go install golang.org/x/tools/gopls@latest` |
| **Lua** | `lua-mode`, `lua-ts-mode` | lua-language-server | `brew install lua-language-server` |
| **Bash** | `sh-mode`, `bash-ts-mode` | bash-language-server | `npm i -g bash-language-server` |
| **PHP** | `php-mode`, `php-ts-mode` | phpactor | `composer global require phpactor/phpactor` |

`web-mode` sends the `html` language id to the HTML server, so embedded
JavaScript/CSS is not handled by a second server; use the corresponding
`js-ts-mode`/`css-mode` buffer for full JS/TS/CSS features.

rust-analyzer only offers type-aware completion and diagnostics when the file
belongs to a Cargo project; for a standalone `.rs` file it runs in
detached-file mode and returns no completions even though Eglot reports the
buffer as managed.  Rust solutions created by the bundled Codeforces client
therefore get a generated `Cargo.toml` whose `[[bin]] path` points at
`solution.rs` (re-opening a problem with `C-c C-c` repairs older workspaces).
For unrelated standalone Rust files, create a Cargo project around the file or
add a `Cargo.toml` with a matching `[[bin]]` entry.  `M-x riven/eglot-diagnose`
flags this case explicitly.

Modes not listed here keep eglot's built-in defaults (no auto-install): Ruby,
Haskell, Scala, Dart, Elixir, Erlang, … — eglot ships contacts for these out
of the box.  Swift uses a custom `sourcekit-lsp` contact.

---

## TypeScript SDK Resolution (Vue / Volar)

Volar needs the path to TypeScript's `lib/` directory. `riven/eglot--vue-tsdk`
resolves it in this priority order:

1. **Project-local**: `<project>/node_modules/typescript/lib`
2. **nvm**: `~/.nvm/versions/node/*/lib/node_modules/typescript/lib`
3. **Active npm global**: `$(npm root -g)/typescript/lib`
4. **Homebrew Intel**: `/usr/local/lib/node_modules/typescript/lib`
5. **Homebrew ARM**: `/opt/homebrew/lib/node_modules/typescript/lib`

If none is found, you're prompted to install `typescript` globally; the tsdk is
re-resolved after install.

---

## Per-Language Modules

Beyond LSP, these modules add language-specific tooling:

| File | Extras |
|------|--------|
| `lisp/lang/init-python.el` | `python-ts-mode`, Ruff flymake backend, `pyvenv` (virtualenv), `pet`. Keys `C-c l c/r/t` send buffer/region/run pytest. |
| `lisp/lang/init-rust.el` | `rust-ts-mode` (indent 2), `cargo` minor mode. |
| `lisp/lang/init-web.el` | `emmet-mode`, `web-mode` (html/ejs, HTML LSP), `vue-ts-mode`, ts/tsx/js/jsx auto-mode (`.jsx` → `js-ts-mode`). React/JSX tree-sitter helpers under `C-c C-e` (`r` rename-tag, `j` jump-to-matching-tag, `d` delete-tag). JSX-aware commenting (`riv/jsx-comment-dwim` on `M-;`). |
| `lisp/lang/init-java.el` | `kotlin-ts-mode` (indent 2) for `.kt`/`.kts`. Java uses builtin `java-ts-mode` + eglot jdtls. |
| `lisp/lang/init-swift.el` | `swift-mode`, `find-sourcekit-lsp` locator. |

---

## Tree-sitter

`lisp/parser/init-treesit.el` configures the managed grammar sources, including
`markdown` and `markdown-inline` for `markdown-ts-appear`. On Emacs 31+ it
uses the native `treesit-enabled-modes` / `treesit-major-mode-remap-alist`
(replacing the older `treesit-auto` approach).

Install grammars:

```elisp
M-x install-essential-treesit-grammars   ;; curated set
M-x install-treesit-language-grammars     ;; all 31 (alias: auto-install-treesit-grammars)
```

Or enable auto-install at startup via `rivenEmacs-auto-install-treesit` (default `nil`).

---

## Diagnostics

`lisp/ide/init-checker.el` configures **flymake** (built-in):

- Debounced current-line diagnostic echoed in the minibuffer.
- Colored mode-line counters (errors/warnings/notes).
- `flymake-eslint` for JS/TS — auto-detects local or global eslint, prefers
  `oxlint` when available.
- `M-x eslint-fix` to apply fixes.

---

## Debugging

`lisp/ide/init-debugger.el` configures **dape** (DAP client) under the `C-c D`
prefix. Saves/loads breakpoints across sessions, pulses the source line on hit,
and uses a gud-style window layout. Debug adapter definitions come from the
`debug-adapters/` git submodule. (`C-c D` opens the debugger; the DeepSeek Harness client moved to `C-c a d`
instead; see [AI-Integration](AI-Integration.md#5-dsh-emacs--deepseek-harness-client).)
