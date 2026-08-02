# Keybindings Reference

RivenEmacs uses **`C-c` as the structured command entry** (not `SPC`).
All leader bindings are declared in spec files under `lisp/keymaps/` and applied
by the declarative engine at `after-init-hook`.

> Tip: press `C-c` then wait 1 s — `which-key` shows every available prefix.

---

## `C-c` Leader Prefixes

### `C-c f` — File

| Key | Action |
|-----|--------|
| `C-c f f` | find file (consult-fd) |
| `C-c f F` | find file from home |
| `C-c f r` | recent file |
| `C-c f d` | recent directory |
| `C-c f s` | deadgrep |
| `C-c f g` | git grep |
| `C-c f b` | ibuffer |
| `C-c f p` | copy buffer path |
| `C-c f n` | rename file/buffer |
| `C-c f D` | delete file/buffer |
| `C-c f R` | revert buffer |
| `C-c f e` | change encoding |
| `C-c f m` | bookmark set |
| `C-c f h` | vundo (undo history) |

### `C-c p` — Project

| Key | Action |
|-----|--------|
| `C-c p p` | switch project |
| `C-c p f` | find file in project |
| `C-c p b` | project buffers |
| `C-c p s` | ripgrep in project |
| `C-c p g` | git grep in project |
| `C-c p r` | color-rg search/replace |
| `C-c p c` | clean invalid buffers |
| `C-c p z` | forget zombie project |
| `C-c p d` | project dired |

### `C-c s` — Search

| Key | Action |
|-----|--------|
| `C-c s l` | line |
| `C-c s i` | imenu |
| `C-c s o` | outline |
| `C-c s m` / `M` | mark / global mark |
| `C-c s c` | avy char-2 |
| `C-c s k` | keep lines |
| `C-c s u` | focus lines |
| `C-c s h` | symbol-overlay |
| `C-c s /` | ripgrep |

### `C-c g` — Git

| Key | Action |
|-----|--------|
| `C-c g g` | magit-status |
| `C-c g b` | magit-blame |
| `C-c g d` | magit-diff |
| `C-c g l` | magit-log |
| `C-c g c` | magit-clone |
| `C-c g i` | magit-init |
| `C-c g s` | magit-stage |
| `C-c g h` / `n` / `p` | diff-hl hunk prev/next |
| `C-c g t` | git-timemachine |

### `C-c c` — Code (LSP)

Bound in `eglot-mode-map`, so active in LSP-managed buffers.

| Key | Action |
|-----|--------|
| `C-c c d` | find definition (`M-.`) |
| `C-c c D` | find declaration |
| `C-c c r` | find references (`M-?`) |
| `C-c c i` | find implementation |
| `C-c c t` | find type definition |
| `C-c c a` | code actions |
| `C-c c f` | format (`riven/eglot-format-dispatch`) |
| `C-c c R` | rename symbol |
| `C-c c k` | kill smart line |
| `C-c c s` | splice sexp |
| `C-c c w` | rewrap sexp |
| `C-c c q` | quickrun |

### `C-c e` — Error / Diagnostics

| Key | Action |
|-----|--------|
| `C-c e l` | list diagnostics |
| `C-c e n` / `p` | next / prev error |
| `C-c e s` | flymake start |
| `C-c e v` | flymake log |
| `C-c e f` | eslint-fix |

### `C-c t` — Tool

| Key | Action |
|-----|--------|
| `C-c t t` | terminal (ghostel) |
| `C-c t d` | docker |
| `C-c t r` | reader (epub) |
| `C-c t n` | elfeed (RSS) |
| `C-c t q` | quickrun |
| `C-c t D` | devdocs |
| `C-c t .` | fanyi dictionary |
| `C-c t m` | woman (man page) |
| `C-c t g` | google search |
| `C-c t T` | google translate |
| `C-c t o` | open in system explorer |

### `C-c w` — Window

| Key | Action |
|-----|--------|
| `C-c w s` | split below |
| `C-c w v` | split right |
| `C-c w d` | delete window |
| `C-c w o` | delete other windows |
| `C-c w w` | other window |
| `C-c w b` | balance windows |
| `C-c w u` / `r` | winner undo / redo |

### `C-c a` — AI

| Key | Action |
|-----|--------|
| `C-c a a` | ai-code-menu |
| `C-c a c` | gptel console |
| `C-c a m` | gptel-menu |
| `C-c a s` | gptel send |
| `C-c a r` | rewrite region |
| `C-c a t` | translate region (review) |
| `C-c a S` | summarize region (review) |
| `C-c a w` | write article |
| `C-c a d` | ask document |
| `C-c a g` | generate commit message |
| `C-c a M` | MCP connect popular |
| `C-c a V` | MCP verify |

### `C-c =` — AI Code Agent

| Key | Action |
|-----|--------|
| `C-c = =` | ai-code-menu |
| `C-c = 1` | claude-code |
| `C-c = 2` | opencode |
| `C-c = 3` | cursor-cli |

### `C-c n` — Note (Org)

| Key | Action |
|-----|--------|
| `C-c n a` | org-agenda |
| `C-c n c` | org-capture |
| `C-c n s` | org-ql-search |
| `C-c n t` | org todo |
| `C-c n d` | deadline |
| `C-c n e` | effort |
| `C-c n p` | property |
| `C-c n r` | refile |
| `C-c n l` | insert link |
| `C-c n f` | footnote |
| `C-c n x` | export |
| `C-c n w` | report last-week tasks |

### `C-c x` — Session

| Key | Action |
|-----|--------|
| `C-c x s` | save session |
| `C-c x l` | load session |
| `C-c x r` | reload session |
| `C-c x c` | current session |
| `C-c x k` | clear session |
| `C-c x d` | delete session |

---

## Navigation `M-g`

| Key | Action |
|-----|--------|
| `M-g c` | avy char-2 |
| `M-g g` | goto line |
| `M-g i` | imenu |
| `M-g l` | goto link |
| `M-g o` | outline |
| `M-g m` / `M` | mark / global mark |
| `M-g n` / `p` | next / prev error |
| `M-g [` / `]` | diff-hl hunk prev/next |

---

## Global / Mode Bindings

| Key | Action | Where |
|-----|--------|-------|
| `M-*` | ai-code-menu | global |
| `M-.` / `M-,` / `M-?` | xref find-def / back / refs | LSP buffers |
| `M-y` | consult-yank-pop | global |
| `C-x b` | consult-buffer | global |
| `C-.` | embark-act | global |
| `C-S-i` | apheleia format | prog |
| `M-$` / `C-M-$` | jinx correct / languages | global |
| `C-=` | expand-region | global |
| `C-;` | iedit | global |
| `M-i` | treesit-fold toggle | prog |
| `M-_` | vundo | global |
| `M-[` | goto-last-change | global |
| `C-x o/1/2/3/0` | switch-window variants | global |
| `C-x d` | dirvish | global |
| `C-h` | diff-hl-show-hunk | prog |
| `C-c m` | smerge hydra | merge buffers |
| `C-c C-e` | JSX tag ops (rename/jump/delete) | tsx-ts-mode |
| `C-c l` | markdown insert/menu hydra | markdown-mode |
| `C-c C-p` | grip live preview | markdown-mode |
| `<f5>` | toggle ef-themes | global |
| `C-x =` | gptel refactor | global |
| `C-c d` | dape debug | prog |
| `M-0..M-9` | indexed candidate insert | corfu active |
