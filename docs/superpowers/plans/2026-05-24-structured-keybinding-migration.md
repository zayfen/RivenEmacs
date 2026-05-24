# Structured Keybinding Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Migrate RivenEmacs global `C-c` keybindings to the approved noun-first `C-c <group> <action>` structure.

**Architecture:** Keep the declarative keybinding engine as the single owner of global `C-c` bindings. Move old Buffer/Open/Query/Session meanings into structured File/Tool/Session groups, add a focused helper module for file-backed buffer commands, and leave `M-g` plus `C-c =` as intentional exceptions.

**Tech Stack:** Emacs Lisp, `keymap-global-set`, `which-key`, built-in `project.el`, ERT.

---

## File Structure

- Create `lisp/keymaps/keybindings-commands.el`: small interactive helper commands used only by the keybinding layer for current file/buffer operations, project root dired, and symbol-overlay dispatch.
- Create `tests/test-structured-keybindings.el`: focused ERT coverage for the new noun groups, retired old prefixes, helper command availability, and duplicate spec entries.
- Modify `lisp/keymaps/keybindings-spec-core.el`: rewrite core groups as File, Project, Search, Git, Code, Error, Tool, Window; remove old Buffer/Find/Edit/Open/Query specs.
- Modify `lisp/keymaps/keybindings-spec-ai.el`: update AI action keys to match the approved layout and keep Agent under `C-c =`.
- Modify `lisp/keymaps/keybindings-spec-org.el`: trim Note group to approved keys and move Session from `C-c s` to `C-c x`.
- Modify `lisp/keymaps/keybindings-spec.el`: require helper commands and merge all global leader groups.
- Modify `lisp/keymaps/keybindings-engine.el`: remove Open/Query application and keep simple-spec application only for Agent if AI becomes part of the global leader spec.
- Modify `lisp/keymaps/init-keybindings.el`: call the updated apply functions and keep Eglot-local `C-c c` support aligned with the global Code group.
- Modify `tests/test-init-session.el`: expect Session under `C-c x`.

---

### Task 1: Red Tests for Structured Layout

**Files:**
- Create: `tests/test-structured-keybindings.el`
- Modify: `tests/test-init-session.el`

- [ ] **Step 1: Add failing structured keybinding tests**

Create `tests/test-structured-keybindings.el` with these tests:

```elisp
;;; test-structured-keybindings.el --- Structured keybinding tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-keybindings)

(defmacro riven/test-with-quiet-keybinding-diagnostics (&rest body)
  "Evaluate BODY without missing-command diagnostics from unloaded packages."
  `(cl-letf (((symbol-function 'riven/keybindings--warn-missing-command) #'ignore))
     ,@body))

(defun riven/test-apply-structured-keybindings ()
  "Apply keybindings in a quiet test context."
  (riven/test-with-quiet-keybinding-diagnostics
   (riven/keybindings-config)))

(ert-deftest riven/structured-keybindings-file-group ()
  "File group owns file and file-backed buffer commands."
  (riven/test-apply-structured-keybindings)
  (dolist (entry '(("C-c f f" . consult-fd)
                   ("C-c f F" . +consult-fd-in-home)
                   ("C-c f r" . consult-recent-file)
                   ("C-c f d" . crux-recentf-find-directory)
                   ("C-c f s" . deadgrep)
                   ("C-c f g" . consult-git-grep)
                   ("C-c f b" . ibuffer-list-buffers)
                   ("C-c f p" . riven/keybindings-copy-current-file-path)
                   ("C-c f n" . riven/keybindings-rename-current-file-or-buffer)
                   ("C-c f D" . riven/keybindings-delete-current-file-or-buffer)
                   ("C-c f R" . revert-buffer-quick)
                   ("C-c f e" . set-buffer-file-coding-system)
                   ("C-c f m" . bookmark-set)
                   ("C-c f h" . vundo)))
    (should (eq (key-binding (kbd (car entry))) (cdr entry)))))

(ert-deftest riven/structured-keybindings-project-search-and-session-groups ()
  "Project, Search, and Session groups use noun prefixes."
  (riven/test-apply-structured-keybindings)
  (dolist (entry '(("C-c p p" . project-switch-project)
                   ("C-c p f" . project-find-file)
                   ("C-c p s" . consult-ripgrep-ex)
                   ("C-c p d" . riven/keybindings-project-dired)
                   ("C-c s l" . consult-line)
                   ("C-c s i" . consult-imenu)
                   ("C-c s o" . consult-outline)
                   ("C-c s c" . avy-goto-char-2)
                   ("C-c s /" . consult-ripgrep)
                   ("C-c x s" . rivenEmacs-session-save)
                   ("C-c x l" . rivenEmacs-session-load)
                   ("C-c x d" . rivenEmacs-session-delete)))
    (should (eq (key-binding (kbd (car entry))) (cdr entry)))))

(ert-deftest riven/structured-keybindings-code-error-tool-window-ai-agent-groups ()
  "Remaining top-level groups expose representative approved commands."
  (riven/test-apply-structured-keybindings)
  (dolist (entry '(("C-c g g" . magit-status)
                   ("C-c g n" . diff-hl-next-hunk)
                   ("C-c g p" . diff-hl-previous-hunk)
                   ("C-c c d" . riven/xref-find-definitions-or-search)
                   ("C-c c D" . riven/eglot-find-declaration-dispatch)
                   ("C-c c a" . eglot-code-actions)
                   ("C-c c s" . sp-splice-sexp)
                   ("C-c c q" . quickrun)
                   ("C-c e l" . riven/flymake-show-buffer-diagnostics-focus)
                   ("C-c e n" . flymake-goto-next-error)
                   ("C-c t t" . eat)
                   ("C-c t d" . docker)
                   ("C-c t ." . fanyi-dwim)
                   ("C-c w s" . split-window-below)
                   ("C-c w u" . winner-undo)
                   ("C-c a a" . ai-code-menu)
                   ("C-c a c" . gptel)
                   ("C-c a S" . riven/gptel-summarize-region-review)
                   ("C-c = =" . agent-shell)))
    (should (eq (key-binding (kbd (car entry))) (cdr entry)))))

(ert-deftest riven/structured-keybindings-retire-old-semantic-prefixes ()
  "Old Buffer/Open/Query/Session meanings are not kept as aliases."
  (riven/test-apply-structured-keybindings)
  (dolist (entry '(("C-c b l" . ibuffer-list-buffers)
                   ("C-c o t" . eat)
                   ("C-c o d" . docker)
                   ("C-c q d" . devdocs-lookup)
                   ("C-c q ." . fanyi-dwim)
                   ("C-c s s" . rivenEmacs-session-save)
                   ("C-c s d" . rivenEmacs-session-delete)))
    (should-not (eq (key-binding (kbd (car entry))) (cdr entry)))))

(ert-deftest riven/structured-keybindings-clear-stale-owned-prefixes ()
  "Reapplying keybindings clears stale commands from retired owned prefixes."
  (dolist (entry '(("C-c b l" . ibuffer-list-buffers)
                   ("C-c o t" . eat)
                   ("C-c q d" . devdocs-lookup)
                   ("C-c s d" . rivenEmacs-session-delete)
                   ("C-c ! l" . flymake-show-buffer-diagnostics)
                   ("C-c e u" . sp-splice-sexp)))
    (keymap-global-set (car entry) (cdr entry)))
  (riven/test-apply-structured-keybindings)
  (dolist (entry '(("C-c b l" . ibuffer-list-buffers)
                   ("C-c o t" . eat)
                   ("C-c q d" . devdocs-lookup)
                   ("C-c s d" . rivenEmacs-session-delete)
                   ("C-c ! l" . flymake-show-buffer-diagnostics)
                   ("C-c e u" . sp-splice-sexp)))
    (should-not (eq (key-binding (kbd (car entry))) (cdr entry)))))

(ert-deftest riven/structured-keybinding-helper-commands-are-interactive ()
  "Helper commands used by specs exist and are interactive."
  (dolist (command '(riven/keybindings-copy-current-file-path
                     riven/keybindings-rename-current-file-or-buffer
                     riven/keybindings-delete-current-file-or-buffer
                     riven/keybindings-project-dired
                     riven/keybindings-symbol-overlay-dispatch))
    (should (fboundp command))
    (should (commandp command))))

(ert-deftest riven/structured-keybinding-spec-has-no-conflicting-duplicates ()
  "Declarative leader specs do not contain conflicting duplicate group actions."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (group riven/keybindings-leader-spec)
      (pcase-let ((`(,prefix ,_title ,bindings) group))
        (dolist (entry bindings)
          (pcase-let* ((`(,key ,cmd ,_wk) entry)
                       (id (format "%s:%s" prefix key))
                       (previous (gethash id seen)))
            (when previous
              (should (eq previous cmd)))
            (puthash id cmd seen))))))
  (should (= (hash-table-count seen)
             (apply #'+
                    (mapcar (lambda (group) (length (nth 2 group)))
                            riven/keybindings-leader-spec)))))

(provide 'test-structured-keybindings)
;;; test-structured-keybindings.el ends here
```

- [ ] **Step 2: Update the session keybinding test expectation**

In `tests/test-init-session.el`, change `rivenEmacs-session-test-declarative-keybindings` to assert `C-c x`:

```elisp
(ert-deftest rivenEmacs-session-test-declarative-keybindings ()
  "Session commands are bound under `C-c x' by the declarative engine."
  (cl-letf (((symbol-function 'riven/keybindings--warn-missing-command) #'ignore))
    (riven/keybindings-apply-leader-spec))
  (dolist (entry '(("C-c x s" . rivenEmacs-session-save)
                   ("C-c x l" . rivenEmacs-session-load)
                   ("C-c x r" . rivenEmacs-session-reload)
                   ("C-c x c" . rivenEmacs-session-current)
                   ("C-c x k" . rivenEmacs-session-clear)
                   ("C-c x d" . rivenEmacs-session-delete)))
    (should (eq (key-binding (kbd (car entry))) (cdr entry)))))
```

- [ ] **Step 3: Run tests to verify RED**

Run:

```bash
emacs --batch \
  -l tests/test-init-session.el \
  -l tests/test-structured-keybindings.el \
  -f ert-run-tests-batch-and-exit
```

Expected: FAIL because helper commands are not defined, Session is still on `C-c s`, and the old groups still exist.

---

### Task 2: Helper Commands

**Files:**
- Create: `lisp/keymaps/keybindings-commands.el`
- Modify: `lisp/keymaps/keybindings-spec.el`

- [ ] **Step 1: Add file/buffer helper commands**

Create `lisp/keymaps/keybindings-commands.el`:

```elisp
;;; keybindings-commands.el --- Commands used by keybinding specs -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused interactive helpers for structured keybinding specs.

;;; Code:

(require 'project)
(require 'subr-x)

(defun riven/keybindings-current-file ()
  "Return the current buffer file name, or nil for non-file buffers."
  (buffer-file-name (buffer-base-buffer)))

(defun riven/keybindings-copy-current-file-path ()
  "Copy the current file path to the kill ring and echo it.
Signal a user error when the current buffer is not visiting a file."
  (interactive)
  (let ((file (riven/keybindings-current-file)))
    (unless file
      (user-error "Current buffer is not visiting a file"))
    (let ((path (file-truename file)))
      (kill-new path)
      (message "%s" path))))

(defun riven/keybindings-rename-current-file-or-buffer (new-name)
  "Rename the current file-backed buffer or rename a non-file buffer to NEW-NAME."
  (interactive
   (list
    (read-string
     (if (riven/keybindings-current-file)
         "Rename file to: "
       "Rename buffer to: ")
     (if-let* ((file (riven/keybindings-current-file)))
         (file-name-nondirectory file)
       (buffer-name)))))
  (if-let* ((file (riven/keybindings-current-file)))
      (let* ((target (expand-file-name new-name (file-name-directory file)))
             (target-dir (file-name-directory target)))
        (when (file-exists-p target)
          (user-error "Target already exists: %s" (abbreviate-file-name target)))
        (unless (file-directory-p target-dir)
          (make-directory target-dir t))
        (rename-file file target)
        (set-visited-file-name target t t)
        (message "Renamed to %s" (abbreviate-file-name target)))
    (rename-buffer new-name t)
    (message "Renamed buffer to %s" new-name)))

(defun riven/keybindings-delete-current-file-or-buffer ()
  "Delete the current file and kill its buffer, or kill a non-file buffer."
  (interactive)
  (if-let* ((file (riven/keybindings-current-file)))
      (when (yes-or-no-p (format "Delete file %s? " (abbreviate-file-name file)))
        (delete-file file)
        (kill-buffer (current-buffer))
        (message "Deleted %s" (abbreviate-file-name file)))
    (kill-buffer (current-buffer))))

(defun riven/keybindings-project-root ()
  "Return current project root, or `default-directory' when outside a project."
  (if-let* ((project (project-current nil)))
      (project-root project)
    default-directory))

(defun riven/keybindings-project-dired ()
  "Open Dired at the current project root or `default-directory'."
  (interactive)
  (dired (riven/keybindings-project-root)))

(defun riven/keybindings-symbol-overlay-dispatch ()
  "Run `symbol-overlay-put' when available, otherwise search symbol at point."
  (interactive)
  (cond
   ((fboundp 'symbol-overlay-put)
    (call-interactively #'symbol-overlay-put))
   ((fboundp 'consult-ripgrep-ex)
    (call-interactively #'consult-ripgrep-ex))
   (t
    (user-error "No symbol highlight/search command is available"))))

(provide 'keybindings-commands)
;;; keybindings-commands.el ends here
```

- [ ] **Step 2: Require helper commands from the spec aggregator**

In `lisp/keymaps/keybindings-spec.el`, add:

```elisp
(require 'keybindings-commands)
```

before requiring spec files.

- [ ] **Step 3: Run helper command tests**

Run:

```bash
emacs --batch -l tests/test-structured-keybindings.el \
  --eval "(ert-run-tests-batch-and-exit 'riven/structured-keybinding-helper-commands-are-interactive)"
```

Expected: PASS.

---

### Task 3: Rewrite Declarative Specs

**Files:**
- Modify: `lisp/keymaps/keybindings-spec-core.el`
- Modify: `lisp/keymaps/keybindings-spec-ai.el`
- Modify: `lisp/keymaps/keybindings-spec-org.el`
- Modify: `lisp/keymaps/keybindings-spec.el`

- [ ] **Step 1: Replace core groups**

Rewrite `riven/keybindings-leader-spec-core` in `lisp/keymaps/keybindings-spec-core.el` so it contains these groups:

```elisp
(defvar riven/keybindings-leader-spec-core
  '(("f" "File"
     (("f" consult-fd "Find file")
      ("F" +consult-fd-in-home "Find file from home")
      ("r" consult-recent-file "Recent file")
      ("d" crux-recentf-find-directory "Recent directory")
      ("s" deadgrep "Search file contents")
      ("g" consult-git-grep "Git grep file contents")
      ("b" ibuffer-list-buffers "List buffers")
      ("p" riven/keybindings-copy-current-file-path "Copy current file path")
      ("n" riven/keybindings-rename-current-file-or-buffer "Rename file or buffer")
      ("D" riven/keybindings-delete-current-file-or-buffer "Delete file or buffer")
      ("R" revert-buffer-quick "Revert buffer")
      ("e" set-buffer-file-coding-system "Set file encoding")
      ("m" bookmark-set "Bookmark")
      ("h" vundo "Buffer history")))

    ("p" "Project"
     (("p" project-switch-project "Switch project")
      ("f" project-find-file "Find project file")
      ("b" consult-project-buffer "Project buffers")
      ("s" consult-ripgrep-ex "Search project text")
      ("g" consult-git-grep "Git grep project")
      ("r" color-rg-search-symbol-in-project "Search/replace in project")
      ("c" +remove-invalidate-buffers "Clean invalid buffers")
      ("z" project-forget-zombie-projects "Forget zombie projects")
      ("d" riven/keybindings-project-dired "Project Dired")))

    ("s" "Search"
     (("l" consult-line "Search line")
      ("i" consult-imenu "Imenu")
      ("o" consult-outline "Outline")
      ("m" consult-mark "Mark")
      ("M" consult-global-mark "Global mark")
      ("c" avy-goto-char-2 "Goto char")
      ("k" consult-keep-lines "Keep matching lines")
      ("u" consult-focus-lines "Focus matching lines")
      ("h" riven/keybindings-symbol-overlay-dispatch "Highlight/search symbol")
      ("/" consult-ripgrep "Ripgrep")))

    ("g" "Git"
     (("g" magit-status "Status")
      ("b" magit-blame "Blame")
      ("d" magit-diff-dwim "Diff")
      ("l" magit-log "Log")
      ("c" magit-clone "Clone")
      ("i" magit-init "Init")
      ("s" magit-stage "Stage")
      ("h" diff-hl-show-hunk "Show hunk")
      ("n" diff-hl-next-hunk "Next hunk")
      ("p" diff-hl-previous-hunk "Previous hunk")
      ("t" git-timemachine-toggle "Time machine")))

    ("c" "Code"
     (("d" riven/xref-find-definitions-or-search "Find definition")
      ("D" riven/eglot-find-declaration-dispatch "Find declaration")
      ("r" xref-find-references "Find references")
      ("i" eglot-find-implementation "Find implementation")
      ("t" riven/eglot-find-type-definition-dispatch "Find type definition")
      ("a" eglot-code-actions "Code action")
      ("f" riven/eglot-format-dispatch "Format buffer")
      ("R" eglot-rename "Rename symbol")
      ("k" sp-kill-whole-line "Kill smart line")
      ("s" sp-splice-sexp "Splice sexp")
      ("w" sp-rewrap-sexp "Rewrap sexp")
      ("q" quickrun "Quickrun")))

    ("e" "Error"
     (("l" riven/flymake-show-buffer-diagnostics-focus "List diagnostics")
      ("n" flymake-goto-next-error "Next error")
      ("p" flymake-goto-prev-error "Previous error")
      ("s" flymake-start "Start checker")
      ("v" flymake-switch-to-log-buffer "View checker log")
      ("f" eslint-fix "Fix with ESLint")))

    ("t" "Tool"
     (("t" eat "Terminal")
      ("d" docker "Docker")
      ("r" reader-open-doc "Reader")
      ("n" elfeed "News")
      ("q" quickrun "Quickrun")
      ("D" devdocs-lookup "DevDocs")
      ("." fanyi-dwim "Dictionary")
      ("m" woman "Manual")
      ("g" riven/google-search "Google search")
      ("T" riven/google-translate "Google translate")
      ("o" +open-in-system-explorer "Open in system explorer")))

    ("w" "Window"
     (("s" split-window-below "Split below")
      ("v" split-window-right "Split right")
      ("d" delete-window "Delete window")
      ("o" delete-other-windows "Delete other windows")
      ("w" other-window "Other window")
      ("b" balance-windows "Balance windows")
      ("u" winner-undo "Winner undo")
      ("r" winner-redo "Winner redo"))))
  "Core declarative specs for structured `C-c' groups.")
```

Delete `riven/keybindings-open-spec` and `riven/keybindings-query-spec` from the same file.

- [ ] **Step 2: Update AI spec keys**

In `lisp/keymaps/keybindings-spec-ai.el`, replace `riven/keybindings-ai-spec` with:

```elisp
(defvar riven/keybindings-ai-spec
  '(("a" ai-code-menu "AI Code")
    ("c" gptel "Console")
    ("m" gptel-menu "Menu")
    ("s" gptel-send "Send")
    ("r" gptel-rewrite "Rewrite")
    ("t" riven/gptel-translate-region-review "Translate")
    ("S" riven/gptel-summarize-region-review "Summarize")
    ("w" gptel-rewrite-article "Write Article")
    ("d" gptel-extensions-ask-document "Ask Document")
    ("g" gptel-generate-commit-message "Generate Commit Message")
    ("M" riven/gptel-mcp-connect-popular "MCP Connect")
    ("V" riven/gptel-mcp-verify "MCP Verify"))
  "Declarative specs for commands under `C-c a'.")
```

- [ ] **Step 3: Move AI into the leader spec and move Session to `C-c x`**

In `lisp/keymaps/keybindings-spec-org.el`, make the Note group use the approved keys and change the session group prefix to `"x"`:

```elisp
(defvar riven/keybindings-leader-spec-org
  '(("n" "Note"
     (("a" org-agenda-list "Agenda")
      ("c" org-capture "Capture")
      ("s" org-ql-search "Search notes")
      ("t" org-todo "Todo")
      ("d" org-deadline "Deadline")
      ("e" org-set-effort "Effort")
      ("p" org-set-property "Property")
      ("r" org-refile "Refile")
      ("l" org-insert-link "Insert link")
      ("f" org-footnote-action "Footnote")
      ("x" org-export-dispatch "Export")
      ("w" report-last-week-tasks "Last week tasks"))))
  "Org declarative specs for structured `C-c' groups.")

(defvar riven/keybindings-leader-spec-session
  '(("x" "Session"
     (("s" rivenEmacs-session-save "Save session")
      ("l" rivenEmacs-session-load "Load session")
      ("r" rivenEmacs-session-reload "Reload session")
      ("c" rivenEmacs-session-current "Current session")
      ("k" rivenEmacs-session-clear "Clear session")
      ("d" rivenEmacs-session-delete "Delete session"))))
  "Session declarative specs for structured `C-c' groups.")
```

In `lisp/keymaps/keybindings-spec.el`, append AI as a leader group:

```elisp
(defvar riven/keybindings-leader-spec
  (append riven/keybindings-leader-spec-core
          `(("a" "AI" ,riven/keybindings-ai-spec))
          riven/keybindings-leader-spec-org
          riven/keybindings-leader-spec-session)
  "Final merged leader specs.")
```

- [ ] **Step 4: Run structured tests**

Run:

```bash
emacs --batch \
  -l tests/test-init-session.el \
  -l tests/test-structured-keybindings.el \
  -f ert-run-tests-batch-and-exit
```

Expected: FAIL only where engine still applies old Open/Query simple specs or init still calls the old function.

---

### Task 4: Engine Cleanup and Eglot Alignment

**Files:**
- Modify: `lisp/keymaps/keybindings-engine.el`
- Modify: `lisp/keymaps/init-keybindings.el`

- [ ] **Step 1: Replace Open/Query/AI simple application with Agent-only application**

In `lisp/keymaps/keybindings-engine.el`, replace `riven/keybindings-apply-open-query-ai` with:

```elisp
(defun riven/keybindings-apply-agent-spec ()
  "Apply the dedicated Agent spec under `C-c ='."
  (riven/keybindings-apply-simple-spec "=" "agent" "Agent"
                                       riven/keybindings-agent-spec))
```

- [ ] **Step 2: Reset owned prefixes before reapplying specs**

Add this variable and function to `lisp/keymaps/keybindings-engine.el`:

```elisp
(defvar riven/keybindings-owned-c-c-prefixes
  '("!" "=" "a" "b" "c" "e" "f" "g" "n" "o" "p" "q" "s" "t" "w" "x")
  "Top-level `C-c' prefixes owned by RivenEmacs keybinding specs.
This includes retired prefixes so config reloads remove stale bindings.")

(defun riven/keybindings-reset-owned-prefixes ()
  "Clear RivenEmacs-owned global `C-c' prefixes before applying specs."
  (clrhash riven/keybindings--seen)
  (dolist (prefix riven/keybindings-owned-c-c-prefixes)
    (keymap-global-unset (concat "C-c " prefix) t)))
```

- [ ] **Step 3: Update keybinding config caller**

In `lisp/keymaps/init-keybindings.el`, replace:

```elisp
(riven/keybindings-apply-open-query-ai)
```

with:

```elisp
(riven/keybindings-reset-owned-prefixes)
(riven/keybindings-apply-agent-spec)
```

- [ ] **Step 4: Align Eglot-local Code map keys**

In `lisp/keymaps/keybindings-spec-lsp.el`, add declaration dispatch and update `riven/keybindings-lsp-spec` to match the global Code group where possible:

```elisp
(defun riven/eglot-find-declaration-dispatch ()
  "Find declaration with Eglot, fallback to Xref definitions."
  (interactive)
  (if (fboundp 'eglot-find-declaration)
      (call-interactively #'eglot-find-declaration)
    (call-interactively #'xref-find-definitions)))
```

```elisp
(defvar riven/keybindings-lsp-spec
  '(("d" riven/xref-find-definitions-or-search "Find Definition")
    ("D" riven/eglot-find-declaration-dispatch "Find Declaration")
    ("r" xref-find-references "Find References")
    ("i" eglot-find-implementation "Find Implementation")
    ("t" riven/eglot-find-type-definition-dispatch "Find Type Definition")
    ("a" eglot-code-actions "Code Actions")
    ("f" riven/eglot-format-dispatch "Format Buffer")
    ("R" eglot-rename "Rename Symbol")
    ("q" quickrun "Quickrun"))
  "Declarative specs for Eglot-local code keybindings.")
```

- [ ] **Step 5: Run focused tests**

Run:

```bash
emacs --batch \
  -l tests/test-init-session.el \
  -l tests/test-focused-config-simplification.el \
  -l tests/test-structured-keybindings.el \
  -f ert-run-tests-batch-and-exit
```

Expected: PASS.

---

### Task 5: Final Verification, Commit, Push

**Files:**
- All changed files from prior tasks.

- [ ] **Step 1: Run configuration load check**

Run:

```bash
emacs --batch -l init.el --eval "(message \"Config loaded successfully\")"
```

Expected: exits 0 and prints `Config loaded successfully`.

- [ ] **Step 2: Inspect git diff**

Run:

```bash
git diff -- lisp/keymaps tests docs/superpowers/plans/2026-05-24-structured-keybinding-migration.md
git status --short --branch
```

Expected: only intended keymap/test/plan changes plus the pre-existing unrelated runtime files.

- [ ] **Step 3: Commit intended changes**

Run:

```bash
git add docs/superpowers/specs/2026-05-24-structured-keybinding-migration-design.md \
  docs/superpowers/plans/2026-05-24-structured-keybinding-migration.md \
  lisp/keymaps/keybindings-commands.el \
  lisp/keymaps/keybindings-engine.el \
  lisp/keymaps/keybindings-spec-ai.el \
  lisp/keymaps/keybindings-spec-core.el \
  lisp/keymaps/keybindings-spec-lsp.el \
  lisp/keymaps/keybindings-spec-org.el \
  lisp/keymaps/keybindings-spec.el \
  lisp/keymaps/init-keybindings.el \
  tests/test-init-session.el \
  tests/test-structured-keybindings.el
git commit -m "feat(keymaps): migrate C-c bindings to noun groups"
```

Expected: one commit containing the design commit plus implementation if the design commit is not already pushed locally; otherwise one implementation commit.

- [ ] **Step 4: Push**

Run:

```bash
git push
```

Expected: pushes local commits to `origin/main`.

---

## Self-Review

- Spec coverage: File, Project, Search, Git, Code, Error, AI, Agent, Note, Tool, Window, and Session are covered by the spec rewrite and ERT representative binding checks.
- Migration rules: Old `C-c b`, `C-c o`, `C-c q`, and Session-on-`C-c s` aliases are explicitly tested as removed.
- Helper coverage: current file path copy, rename, delete, encoding binding, project Dired, and symbol highlight/search fallback are covered by existence and representative binding tests.
- Duplicate coverage: the final leader spec is scanned for conflicting duplicate prefix/action pairs.
