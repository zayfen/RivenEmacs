# Troubleshooting

Common problems and fixes.

---

## Startup

### Exit asks "Directory for desktop file"

RivenEmacs stores the desktop session in the fixed directory
`~/.cache/emacs/`; it no longer asks where to save on exit. A one-time
migration copies a legacy `local/desktop/.emacs.desktop` into that cache
directory. If a second Emacs instance finds the desktop lock owned by another
process, it keeps the fixed directory but disables session saving for that
instance (instead of prompting or overwriting the owner's session). Inspect the
active file with `M-x rivenEmacs-session-current`; change
`rivenEmacs-session-directory` only if you need a different fixed location.

### Slow startup / frame takes long to appear

Heavy modules (LSP, AI, languages, tools) load on `after-init-hook` at depth
-20: during startup, before desktop restore/keybinding handlers and before the
first redisplay. They are synchronous startup work (not idle-deferred), so they
still contribute to startup time. If startup feels slow:

```bash
emacs -Q --batch -l benchmark-startup.el
```

The numbers in `docs/startup-optimization-report.md` are from an older
configuration; re-run the benchmark locally instead of comparing against them.

### Packages not installing / ELPA errors

RivenEmacs uses **Tsinghua ELPA mirrors**. If you're outside China or the mirror
is down, check connectivity:

```bash
curl -I https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/
```

The first run installs everything into `elpa/`. Delete `elpa/` to force a clean
reinstall.

### `Cannot open load file: ... init-config`

You loaded a module without the `load-path` set up. Load through `init.el` (it
adds the module subdirectories), or use `scripts/run-tests.sh`, which builds the
`load-path` from the repository without loading `init.el`.

---

## LSP / Eglot

### `Can't find typescript.js or tsserverlibrary.js in ""`

The Vue (Volar) server couldn't locate TypeScript's `lib/` directory. This is
fixed in current RivenEmacs: `riven/eglot--vue-tsdk` searches project-local,
nvm, `npm root -g`, and Homebrew paths. If it still happens, install TypeScript
globally:

```bash
npm i -g typescript
```

Then `M-x riven/gptel-mcp-verify` is **not** the right tool — restart Emacs so
the contact re-resolves, or just re-open the file.

### Server not found / "not installed"

Eglot auto-install prompts only fire when `rivenEmacs-eglot-auto-install` is
non-nil (default). If you declined or it's disabled, install the server manually:

| Language | Command |
|----------|---------|
| Python | `pip install basedpyright` (or `npm i -g pyright`) |
| Vue | `npm i -g @vue/language-server @vue/language-core typescript` |
| Rust | `rustup component add rust-analyzer` |
| C/C++ | `brew install llvm` |
| Java | `brew install jdtls` |
| JS/TS | `npm i -g typescript-language-server typescript` |

Verify the binary is on `PATH` with `executable-find`:

```elisp
(executable-find "vue-language-server")   ;; => "/usr/local/bin/..."
```

### `Wrong number of arguments: (0 . 0), 2`

A function-form eglot contact was defined with zero parameters, but eglot calls
it with `(interactive project)`. All RivenEmacs contacts use
`(&optional _interactive _project)` — if you see this, a contact was defined
without that signature.

### LSP looks started but completion does not pop up

Run `M-x riven/eglot-diagnose` in the affected buffer. It reports whether Eglot
really manages the buffer, the server process and its status, the effective
`completion-at-point-functions`, and the Corfu auto-popup state, plus a hint
for the observed failure mode:

- **Not managed** — Eglot never connected. Check the `*EGLOT (...) events*`
  buffer for connection errors or an install prompt that was declined.
- **Managed but no live server** — the mode line outlived the connection; run
  `M-x eglot-reconnect`.
- **Managed with a live server but no candidates** — for Rust this usually
  means rust-analyzer is in detached-file mode because the file has no
  `Cargo.toml`; start the Codeforces problem again with `C-c C-c` (Rust
  workspaces get a generated manifest) or create a Cargo project around the
  file.

Python contacts prefer an installed server before prompting: with
`pyright-langserver` installed, an absent BasedPyright does not trigger an
install prompt.  Declining or quitting one prompt only skips that server.

### lsp-bridge

`lsp-bridge` is no longer part of RivenEmacs: the `init-lsp-bridge.el` module
was removed. **Eglot is the only LSP client**; commands and documentation that
mention `lsp-bridge-*` are historical and do not apply.

---

## Testing

### Tests fail to load modules

Run the tests through the repository runner, which sets `load-path` correctly
and uses the checked-in sources rather than stale byte-code:

```bash
scripts/run-tests.sh                          # all ERT + Python unit tests
scripts/run-tests.sh tests/test-init-terminal.el   # one file
```

### Nested `emacs -l init.el` tests are skipped

The startup-integration tests that spawn a nested Emacs loading `init.el` are
opt-in because they may install packages over the network:

```bash
RIVENEMACS_RUN_INIT_TESTS=1 scripts/run-tests.sh
```

---

## AI

### MCP servers not connecting

```elisp
M-x riven/gptel-mcp-verify   ;; or C-c a V
```

Reports which servers connected and which were skipped. Common causes:

- **Missing API key** — brave-search needs `BRAVE_API_KEY`, tavily needs
  `TAVILY_API_KEY`, github needs `GITHUB_TOKEN`. Set in your shell profile.
- **Binary missing** — some servers need `npx` / node packages. Install Node.js.
- **Toggle off** — check `rivenEmacs-mcp-enable-*-server` defcustoms.

### gptel: no API key / auth error

Keys are read from env → login-shell fallback → `auth-source`. To store a key
in `~/.authinfo.gpg`:

```
machine api.zayfen.com login apikey password ZF_yourkey
```

Check what gptel sees:

```elisp
(getenv "ZAYFEN_API_KEY")
```

### exec-path-from-shell didn't import keys

It runs 2 s after init. If you set an env var in your shell profile and Emacs
was already running, restart Emacs. Verify with:

```bash
bash scripts/riven-deps.sh doctor
```

---

## Tree-sitter

### `treesit-mode not enabled` / no font-lock

Install grammars:

```elisp
M-x install-essential-treesit-grammars
```

Or enable auto-install: `(setq rivenEmacs-auto-install-treesit t)`.

### Native comp errors (`eln-cache`)

The eln-cache is redirected to `local/eln/`. If you see native-comp errors:

```bash
rm -rf local/eln
```

Then restart Emacs.

---

## Fonts / UI

### Icons show as boxes

`nerd-icons` fonts not installed. Run:

```elisp
M-x nerd-icons-install-fonts
```

### CJK / emoji not rendering

`riven/apply-fonts` picks the first available CJK/emoji font. Ensure one of
Sarasa Mono SC, PingFang SC is installed (macOS has PingFang built in).

---

## General

### `y-or-n-p` prompts instead of full yes/no

Intentional — `init-default.el` sets `(defalias 'yes-or-no-p 'y-or-n-p)`.

### State files cluttering the repo

RivenEmacs relocates ~30 built-in state files into `local/` (gitignored).
`custom-file` is `custom.el` at the repo root. Don't edit `init.el` via
`M-x customize` — use `custom.el` or a local module.

### Getting help

- `C-h` then wait — `which-key` shows keybindings.
- `C-h b` — all bindings in current buffer.
- `M-x customize-group RET rivenEmacs RET` — all config options.
