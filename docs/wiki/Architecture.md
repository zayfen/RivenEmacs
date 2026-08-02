# Architecture

RivenEmacs separates **assembly** (`init.el`, `early-init.el`) from
**configuration** (`lisp/` modules). `init.el` contains no feature config — it
only sets up `load-path` and orchestrates module loading in phases.

---

## Startup Flow

```
early-init.el ──► init.el ──► core+UI (immediate) ──► deferred (emacs-startup-hook)
                                  │                          │
                                  └─► first frame (~0.5s)    └─► LSP, AI, langs, tools
```

### 1. `early-init.el` — pre-package optimization

Runs before any package is loaded. Handles:

- **GC deferral** — `gc-cons-threshold` = `most-positive-fixnum` during startup,
  restored to 16 MB after init. Prevents GC pauses during load.
- **`file-name-handler-alist = nil`** during startup (restored after) — skips
  per-file handler lookups.
- **`use-package-always-defer t`** — packages defer by default.
- **native-comp eln-cache** redirected to `local/eln/`.
- **`custom-file`** → `root-dir/custom.el` (keeps `init.el` clean).
- **macOS** — `command` key = `Meta`.
- **Package archives** — Tsinghua ELPA mirrors (China-optimized).

### 2. `init.el` — assembly

~135 lines. Does, in order:

1. **`load-path` construction** — adds `riven/root-dir`, `riven/lisp-dir`, and
   recursively every subdirectory of `lisp/` (except `local`). Every module
   file is reachable by `require`.
2. **`exec-path-from-shell`** — deferred 2 s; imports proxy + AI/MCP API keys
   (`ZAYFEN_API_KEY`, `DEEPSEEK_API_KEY`, `OPENAI_API_KEY`,
   `ANTHROPIC_AUTH_TOKEN`, `BRAVE_API_KEY`, `TAVILY_API_KEY`,
   `GITHUB_TOKEN`, …).
3. **`lisp/env.el`** → `core/env.el` (path constants + OS predicates).
4. **Package system** — sets `package-user-dir` = `elpa/`, mirrors, calls
   `riven/initialize-package-system`.
5. **Loader functions** (`riven/load-*-modules`) each `require` a set of
   modules.
6. **Execution order**:
   - Core + UI + writing + session + tooling loaded **immediately**.
   - **Deferred modules** registered on `emacs-startup-hook` at **priority 90**
     — so the first frame renders before heavy LSP/AI/lang/tool modules load.
   - **Keybindings** load last, on `after-init-hook` inside `init-keybindings.el`.

### Loader → module mapping

| Loader | Loads |
|--------|-------|
| `riven/load-core-modules` | use-package, config, default, helper |
| `riven/load-ui-modules` | theme, font, undo, autosave, which-key, hydra, minibuffer, completion-ui, consult, vertico, crux, editor, dired, format, jump, editorconfig, checker, pair, fold, markdown, treesit |
| `riven/load-writing-modules` | `ews` (immediate) + `init-org` (lazy via `org-mode-hook`) |
| `riven/load-session-modules` | `init-session` |
| `riven/load-tooling-modules` | `init-terminal` |
| `riven/load-keymap-modules` | `init-keybindings` |
| `riven/load-deferred-modules` | eglot, vc, debugger, envrc, project, gpt, ai-code, web, rust, python, java, swift, docker, quickrun, feed, lookup, reader, codeforces |

---

## Lazy-Loading Strategy

The central design principle: **the main path renders the first frame fast**;
everything heavy loads later.

| When it loads | What | How |
|---------------|------|-----|
| Immediate | core, UI, completion, editor, treesit | direct `require` |
| `emacs-startup-hook` (pri 90) | LSP, VCS, AI, languages, tools | `riven/load-deferred-modules` |
| `after-init-hook` | keybindings | engine applies specs |
| First org buffer | `init-org` | `org-mode-hook` |
| 2 s after init | shell env (API keys, proxy) | `exec-path-from-shell` timer |
| Connect time | LSP server binary | function-form eglot contact |

---

## Declarative Keybinding Engine

A signature feature. Keys are **not** scattered across modules — they live as
spec files in `lisp/keymaps/`:

```
keybindings-engine.el      applies specs, detects duplicates, owns C-c prefixes
keybindings-spec.el        aggregates all specs
keybindings-spec-core.el   File/Project/Git/Code/Search/Window/Note/Error
keybindings-spec-ai.el     AI / agent prefixes
keybindings-spec-lsp.el    LSP code keys
keybindings-spec-org.el    Org / session keys
keybindings-commands.el    interactive helpers backing the specs
init-keybindings.el        entry: applies all specs on after-init-hook
```

The engine:
- **Clears owned `C-c` prefixes** before applying (no stale bindings).
- **Registers which-key descriptions** for every prefix group.
- **Warns on duplicate or missing commands** at load time.

See **[Keybindings](Keybindings.md)** for the full reference.

---

## Path Constants

Defined in `lisp/core/env.el`, available everywhere:

| Constant | Meaning |
|----------|---------|
| `root-dir` | Repository root (`/…/RivenEmacs`) |
| `lisp-dir` | `root-dir/lisp` |
| `local-dir` | `root-dir/local` (gitignored, user data) |
| `cache-dir` | `root-dir/local/cache` |
| `repo-dir` | `root-dir/elpa` (package install dir) |

OS predicates: `os/mac`, `os/linux`, `os/bsd`, `os/win`.

---

## Benchmarking

```bash
emacs -Q --batch -l benchmark-startup.el
```

Baseline after optimization: **~0.52 s** to load (vs ~0.56 s pre-optimization).
See `docs/startup-benchmark.md` and `docs/startup-optimization-report.md`.
