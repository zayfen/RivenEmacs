# Structured Keybinding Migration Design

## Goal

Redesign RivenEmacs global `C-c` keybindings around a structured
object-first model:

```text
C-c <noun> <action>
```

Top-level groups are nouns that describe the object or domain being operated
on. Commands inside each group are actions on that object. This is a deliberate
large migration: the old `C-c` layout does not need semantic compatibility.

## Principles

- Top-level groups must be nouns: `File`, `Project`, `Search`, `Git`, `Code`,
  `Error`, `AI`, `Agent`, `Note`, `Tool`, `Window`, `Session`.
- Group actions may be verbs, and common action keys should be reused where the
  group context makes the meaning clear.
- Buffer and file operations belong together under `File`. A command should act
  on the current file when the buffer visits one, otherwise fall back to a
  buffer operation or report a clear user error.
- Existing `C-c =` remains the dedicated Agent prefix because it is already
  established and avoids consuming another noun letter.
- `M-g` navigation may remain as a traditional Emacs goto prefix, but `C-c s`
  becomes the structured Search group.

## Top-Level Groups

```text
C-c f  File
C-c p  Project
C-c s  Search
C-c g  Git
C-c c  Code
C-c e  Error
C-c a  AI
C-c =  Agent
C-c n  Note
C-c t  Tool
C-c w  Window
C-c x  Session
```

## File Group

`C-c f` owns file and file-backed buffer operations.

```text
C-c f f  find file in current/project directory
C-c f F  find file from home/default workspace
C-c f r  recent file
C-c f d  recent directory
C-c f s  search file contents
C-c f g  git grep file contents
C-c f b  list buffers
C-c f p  copy current file path
C-c f n  rename current file/buffer
C-c f D  delete current file/buffer
C-c f R  revert current buffer
C-c f e  set buffer file encoding
C-c f m  bookmark current location/file
C-c f h  buffer history/undo tree
```

Implementation should add small helper commands for file-backed operations that
do not already have a suitable command, especially rename, delete, path copy,
and encoding selection.

## Project Group

`C-c p` owns project-level operations.

```text
C-c p p  switch project
C-c p f  find project file
C-c p b  project buffers
C-c p s  search project text
C-c p g  git grep project
C-c p r  search/replace in project
C-c p c  clean invalid buffers
C-c p z  forget zombie projects
C-c p d  project dired/root
```

The group should use built-in `project.el` and existing local helpers where
possible.

## Search Group

`C-c s` owns search, navigation, and location jumps. It replaces the old Session
prefix.

```text
C-c s l  search line in current buffer
C-c s i  imenu
C-c s o  outline
C-c s m  mark ring
C-c s M  global mark ring
C-c s c  avy goto char
C-c s k  keep matching lines
C-c s u  focus matching lines
C-c s h  symbol highlight/query
C-c s /  ripgrep
```

This group should not contain file management actions.

## Git Group

`C-c g` owns Git and repository operations.

```text
C-c g g  magit status
C-c g b  blame
C-c g d  diff
C-c g l  log
C-c g c  clone
C-c g i  init
C-c g s  stage
C-c g h  show hunk
C-c g n  next hunk
C-c g p  previous hunk
C-c g t  time machine
```

## Code Group

`C-c c` owns code intelligence, code structure, formatting, and execution.

```text
C-c c d  find definition
C-c c D  find declaration
C-c c r  find references
C-c c i  implementation
C-c c t  type definition
C-c c a  code action
C-c c f  format buffer
C-c c R  rename symbol
C-c c k  kill smart line/sexp
C-c c s  splice/delete around sexp
C-c c w  rewrap sexp
C-c c q  quickrun
```

LSP/Eglot/LSP Bridge commands should continue to use dispatch helpers where the
project already has them.

## Error Group

`C-c e` owns diagnostics, errors, lint, and fixes.

```text
C-c e l  list diagnostics
C-c e n  next error
C-c e p  previous error
C-c e s  start checker
C-c e v  view checker log
C-c e f  fix with eslint
```

## AI Group

`C-c a` owns high-level AI workflows.

```text
C-c a a  ai-code menu
C-c a c  gptel console
C-c a m  gptel menu
C-c a s  send to gptel
C-c a r  rewrite
C-c a t  translate
C-c a S  summarize
C-c a w  write article
C-c a d  ask document
C-c a g  generate commit message
C-c a M  MCP connect
C-c a V  MCP verify
```

## Agent Group

`C-c =` remains the dedicated agent backend prefix.

```text
C-c = =  start/reuse agent shell
C-c = 1  start Claude Code
C-c = 2  start OpenCode
C-c = 3  start Cursor ACP
C-c = s  setup/upgrade agent shell
```

## Note Group

`C-c n` owns Org, note, agenda, and capture operations.

```text
C-c n a  agenda
C-c n c  capture
C-c n s  org-ql search
C-c n t  todo
C-c n d  deadline
C-c n e  effort
C-c n p  property
C-c n r  refile
C-c n l  insert link
C-c n f  footnote
C-c n x  export
C-c n w  last week tasks
```

## Tool Group

`C-c t` owns external tools and utility surfaces.

```text
C-c t t  terminal
C-c t d  docker
C-c t r  reader
C-c t n  news/feed
C-c t q  quickrun
C-c t D  devdocs
C-c t .  dictionary
C-c t m  man page
C-c t g  google search
C-c t T  translate
C-c t o  open in system explorer
```

This group replaces the old `Open` and `Query` prefixes.

## Window Group

`C-c w` owns window and layout operations.

```text
C-c w s  split window below
C-c w v  split window right
C-c w d  delete window
C-c w o  delete other windows
C-c w w  other window
C-c w b  balance windows
C-c w u  winner undo
C-c w r  winner redo
```

If `winner-mode` is not currently enabled, the implementation may enable it in
core defaults or make the two winner commands conditional.

## Session Group

`C-c x` owns desktop session and Emacs state commands.

```text
C-c x s  save session
C-c x l  load session
C-c x r  reload session
C-c x c  current session
C-c x k  clear session
C-c x d  delete session
```

This frees `C-c s` for Search.

## Migration Rules

- Delete the old `C-c b` Buffer group and move its useful commands into
  `C-c f`.
- Change old `C-c f` from `Find` to `File`.
- Move Session from `C-c s` to `C-c x`.
- Delete old `C-c o` Open and `C-c q` Query prefixes and move their commands
  into `C-c t`.
- Keep `C-c =` Agent.
- Keep `M-g` as a traditional goto prefix unless it conflicts with the new
  structured map.

## Implementation Notes

- The existing declarative keybinding engine should remain the owner of global
  `C-c` bindings.
- Prefer updating spec data in `lisp/keymaps/keybindings-spec-*.el` over adding
  imperative `keymap-global-set` calls.
- Add helper commands in a focused keybinding helper module if current Emacs
  commands do not have the desired current-file/current-buffer behavior.
- Duplicate-key detection should continue to work through
  `riven/keybindings--register`.

## Testing

Add ERT coverage that verifies:

- Top-level groups bind to the intended representative commands.
- Old prefixes no longer own their previous meanings:
  - `C-c s` is Search, not Session.
  - `C-c o` and `C-c q` no longer expose Open/Query groups.
  - `C-c b` no longer exposes the old Buffer group.
- Session commands are available under `C-c x`.
- File/buffer helper commands exist and are interactive.
- The keybinding spec contains no duplicate prefix/action pairs with different
  commands.

## Acceptance Criteria

- Running the focused keybinding/session ERT suite passes.
- `emacs --batch -l init.el` exits 0.
- New groups appear under the declared `C-c <noun>` prefixes.
- Old semantic groups are removed rather than kept as aliases, except `M-g` and
  `C-c =`.
