# RivenEmacs Wiki

A modern, modular, performance-optimized Emacs configuration built around a
**declarative keybinding engine**, **phased lazy loading**, and first-class
**AI / MCP integration**.

> Leader key is **`C-c`** (not `SPC`). RivenEmacs does not use evil/general.

---

## 📖 Pages

| Page | What it covers |
|------|----------------|
| **[Architecture](Architecture.md)** | Startup flow, `early-init` → `init.el` → phased module loading, lazy-loading strategy |
| **[Modules](Modules.md)** | Complete reference for all 16 module groups under `lisp/` |
| **[Keybindings](Keybindings.md)** | Full `C-c` prefix reference (File, Project, Git, Code, AI, Note, …) |
| **[AI-Integration](AI-Integration.md)** | gptel backends, MCP tool servers, ai-code CLI agents |
| **[Language-Support](Language-Support.md)** | Per-language LSP setup with on-demand server install |
| **[Customization](Customization.md)** | `rivenEmacs-*` defcustoms and environment variables |
| **[Troubleshooting](Troubleshooting.md)** | Common problems and fixes |
| **[Development](Development.md)** | How to contribute: code style, naming, module template |

---

## 🚀 Quick Start

```bash
# 1. Check / install external dependencies
bash scripts/riven-deps.sh doctor    # report what's missing
bash scripts/riven-deps.sh fix       # install them (interactive)

# 2. Launch Emacs — first run installs packages from ELPA
emacs
```

First frame renders in **~0.5 s**; heavy modules (LSP, AI, languages, tools)
load shortly after on `emacs-startup-hook`.

---

## ✨ Highlights

- **Fast startup** — phased lazy loading, GC tuning, native-comp.
- **Declarative keybindings** — all keys live in spec files under `lisp/keymaps/`;
  the engine detects duplicates and missing commands.
- **Eglot as default LSP** with on-demand server install (Python, Vue, Kotlin,
  Rust, C/C++, Java, JS/TS, PHP).
- **AI first-class** — gptel (Zayfen / DeepSeek / OpenAI) + MCP tool servers +
  ai-code CLI agents (Codex / Claude Code / Opencode / Aider).
- **Tree-sitter everywhere** — Emacs 31 native `treesit-enabled-modes`.
- **Bundled extras** — Codeforces client, epub reader, RSS feed, Beamer
  lecture-series export.

---

## 🗂 Repository Layout

```
RivenEmacs/
├── init.el              # assembly entry: load-path + phased require
├── early-init.el        # startup perf (GC, native-comp, package archives)
├── lisp/                # all configuration (16 module groups)
├── emacs-codeforces/    # bundled Codeforces competitive-programming client
├── debug-adapters/      # git submodule — DAP debug adapter definitions
├── scripts/riven-deps.sh  # dependency doctor / installer
├── docs/                # documentation + this wiki
├── tests/               # ERT test suite
├── Fonts/               # bundled fonts
└── snippets/            # yasnippet snippets
```

See **[Modules](Modules.md)** for the full `lisp/` breakdown.
