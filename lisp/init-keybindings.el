;; -*- coding: utf-8; lexical-binding: t -*-

;;; init-keybindings.el --- Keybindings configuration for RivenEmacs

;;; Commentary:
;; This module defines all keybindings for RivenEmacs using general.el.
;; It includes leader key definitions for various modes and functionality
;; including buffer management, code navigation, git operations, project
;; management, and AI assistant integration.

;;; Code:
;; config project keybindings

(defun keybindings-config()
  (require 'agent-shell nil t) ; Ensure agent-shell-leader-def exists
  (progn
    (leader-def
      :infix "b"
      "" '(:ignore t :wk "Buffer")
      "." '(+show-current-buffer-path :wk "Show buffer path")
      "-" '(narrow-to-region :wk "Narrow region")
      "=" '(widen :wk "Widen narrow region")
      "l" '(ibuffer-list-buffers :wk "List buffers")
      "p" '(previous-buffer :wk "Prev buffer")
      "m" '(bookmark-set-position :wk "Mark position")
      "n" '(next-buffer :wk "Next buffer")
      "q" '(read-only-mode :wk "ReadOnly buffer")
      "x" '(revert-buffer-quick :wk "Revert buffer")
      "h" '(vundo :wk "History"))

    ;; LSP-bridge SPC c bindings set in with-eval-after-load 'lsp-bridge

    (leader-def
      :infix "n"
      "" '(:ignore t :wk "Note")
      "a" '(org-agenda-list :wk "Agenda List")
      "c" '(:ignore t :wk "Clock")
      "d" '(org-deadline :wk "Set Deadline")
      "e" '(org-set-effort :wk "Set Effort")
      "f" '(org-footnote-action :wk "Footnote")
      "i" '(org-capture :wk "Capture")
      "l" '(org-insert-link :wk "Insert Link")
      "m" '(org-mark-subtree :wk "Mark Subtree")
      "p" '(org-set-property :wk "Set Property")
      "r" '(org-refile :wk "Refile")
      "s" '(org-ql-search :wk "Search")
      "t" '(org-todo :wk "Toggle TODO")
      "u" '(org-update-statistics-cookies :wk "Update Stats")
      "v" '(org-show-todo-tree :wk "TODO Tree")
      "w" '(report-last-week-tasks :wk "Last Week Tasks")
      "x" '(org-export-dispatch :wk "Export")
      "+" '(org-shifttab :wk "Cycle Visibility")
      "*" '(org-ctrl-c-star :wk "Insert Heading"))

    ;; Clock 命令子组 - C-c n c
    (leader-def
      :infix "nc"
      "" '(:ignore t :wk "Clock")
      "i" '(org-clock-in :wk "Clock in")
      "o" '(org-clock-out :wk "Clock out")
      "g" '(org-clock-goto :wk "Goto current")
      "j" '(org-clock-jump-to-current-clock :wk "Jump to current")
      "s" '(org-clock-display :wk "Show status")
      "r" '(org-clock-report :wk "Generate report")
      "q" '(org-clock-cancel :wk "Cancel current")
      "e" '(org-clock-modify-effort-estimate :wk "Modify effort"))

    ;; TODO 状态子组 - C-c n T
    (leader-def
      :infix "nT"
      "" '(:ignore t :wk "TODO")
      "t" '(org-todo :wk "Toggle State")
      "n" '(org-todo :wk "Next State")
      "p" '(org-todo :wk "Previous State")
      "d" '(org-todo :wk "Set DONE")
      "c" '(org-todo :wk "Set CANCELLED"))

    ;; 折叠/展开子组 - C-c n TAB
    (leader-def
      :infix "nTAB"
      "" '(:ignore t :wk "Fold")
      "TAB" '(org-cycle :wk "Cycle Visibility")
      "h" '(org-hide-block-all :wk "Hide All")
      "s" '(org-show-all :wk "Show All")
      "1" '(org-show-only-headers-1 :wk "Level 1")
      "2" '(org-show-only-headers-2 :wk "Level 2")
      "3" '(org-show-only-headers-3 :wk "Level 3")
      "a" '(org-overview :wk "Overview")
      "c" '(org-content :wk "Content")
      "e" '(org-expand-all :wk "Expand All"))

    ;; 表格子组 - C-c n |
    (leader-def
      :infix "n|"
      "" '(:ignore t :wk "Table")
      "|" '(org-table-create-or-convert-from-region :wk "Create/Convert")
      "a" '(org-table-align :wk "Align")
      "r" '(org-table-recalculate :wk "Recalculate")
      "i" '(org-table-insert-row :wk "Insert Row")
      "I" '(org-table-insert-column :wk "Insert Column")
      "d" '(org-table-delete-row :wk "Delete Row")
      "D" '(org-table-delete-column :wk "Delete Column")
      "s" '(org-table-sort-lines :wk "Sort")
      "f" '(org-table-sort-lines :wk "Sort Field"))

    (leader-def
      :infix "p"
      "" '(:ignore t :wk "Project")
      "c" '(+remove-invalidate-buffers :wk "Clean invalidate buffers")
      "C" '(project-forget-zombie-projects :wk "Forget zombie projects")
      "f" '(project-find-file :wk "Find file in project")
      "g" '(+goto-file-at-point :wk "Goto file at point")
      "p" '(project-switch-project-ex :wk "Switch project")
      "s" '(consult-ripgrep-ex :wk "Search symbol")
      "b" '(consult-project-buffer :wk "Buffers in project")
      "/" '(consult-git-grep :wk "Git grep")
      "r" '(color-rg-search-symbol-in-project :wk "Search/Replace"))

    (leader-def
      :infix "f"
      "" '(:ignore t :wk "Find")
      "d" '(crux-recentf-find-directory :wk "Global Recent directory")
      "f" '(consult-fd :wk "Directory file")
      "l" '(+consult-fd-in-home :wk "Home file")
      "r" '(consult-recent-file :wk "Global Recent files")
      "s" '(deadgrep :wk "String"))

    (leader-def :infix "g"
      "" '(:ignore t :wk "Git")
      "b" '(magit-blame :wk "Blame")
      "d" '(magit-diff-dwim :wk "Diff")
      "c" '(magit-clone :wk "Clone")
      "g" '(magit-status :wk "Status")
      "h" '(git-hunk-toggle-mode :wk "Hunk navigation")
      "i" '(magit-init :wk "Init")
      "l" '(magit-log :wk "Log")
      "s" '(magit-stage :wk "Stage")
      "t" '(git-timemachine-toggle :wk "Time machine")
      "TAB" '(diff-hl-show-hunk :wk "Show hunk"))

    (leader-def
      :infix "k"
      "" '(:ignore t :wk "Kill")
      "a" '(sp-splice-sexp :wk "Delete Around")
      "A" '(sp-rewrap-sexp :wk "Delete Around and Rewrap")
      "k" '(sp-kill-whole-line :wk "Kill line(s) (smart)")
      "r" '(vr/replace :wk "Replace")
      "s" '(sp-delete-sexp :wk "Delete Sexp")
      "z" '(zap-to-char :wk "Zap to char"))

    (open-leader-def
      "" '(:ignore t :wk "Open")
      "d" '(docker :wk "Docker")
      "e" '(+open-in-system-explorer :wk "Explorer")
      "n" '(elfeed :wk "News")
      "q" '(quickrun :wk "Quickrun")
      "r" '(reader-open-doc :wk "Reader")
      "t" '(eat :wk "Terminal"))

    (query-leader-def
      "" '(:ignore t :wk "Query")
      "d" '(devdocs-lookup :wk "DevDocs")
      "." '(fanyi-dwim :wk "Dictionary")
      "m" '(woman :wk "Unix Manual")
      "g" '(riven/google-search :wk "Google")
      "t" '(riven/google-translate :wk "Google Translator"))

    (ai-leader-def
       "" '(:ignore t :wk "AI")
       ;; 基础功能
       "." '(gptel :wk "Console")
       "m" '(gptel-menu :wk "Menu")
       "=" '(gptel-send :wk "Send")
       "r" '(gptel-rewrite :wk "Rewrite")
       "t" '(gptel-translate-region :wk "Translate")
       "?" '(gptel-extensions-ask-document :wk "Ask Document")
       ;; 新增功能
       "w" '(gptel-rewrite-article :wk "Write Article")
       "s" '(gptel-summarize-document :wk "Summarize Document")
       "q" '(gptel-query-devdoc :wk "Query DevDoc")
       "g" '(gptel-generate-commit-message :wk "Generate Commit Message"))

    ;; Session management keybindings (requires init-session)
    ;; Uses easysession's built-in commands directly
    (leader-def
      :infix "s"
      "" '(:ignore t :wk "Session")
      "s" '(easysession-switch-to :wk "Switch session")
      "S" '(easysession-save-as :wk "Save as new")
      "l" '(easysession-load :wk "Load session")
      "L" '(easysession-load-including-geometry :wk "Load with geometry")
      "d" '(easysession-delete :wk "Delete session")
      "r" '(easysession-rename :wk "Rename session")
      "." '(easysession-save :wk "Save current")
      "c" '(easysession-get-current-session-name :wk "Current session"))

    ;; define prefix lable
    (leader-def
      "\!" '(:ignore t :wk "Checker(Flymake)")
      "&" '(:ignore t :wk "Yasnippet")
      "@" '(:ignore t :wk "Hideshow"))

    ;; define flymake keybindings
    (leader-def
      :infix "\!"
      "" '(:ignore t :wk "Checker(Flymake)")
      "l" '(flymake-show-buffer-diagnostics :wk "List diagnostics")
      "n" '(flymake-goto-next-error :wk "Next error")
      "p" '(flymake-goto-prev-error :wk "Previous error")
      "s" '(flymake-start :wk "Start flymake")
      "v" '(flymake-switch-to-log-buffer :wk "View log")
      "f" '(eslint-fix :wk "Fix with ESLint"))

    ;; define navigate to elements keybindings
    (navigate-leader-def
      "c" '(avy-goto-char-2 :wk "Goto Char")
      "g" '(consult-goto-line :wk "Line")
      "i" '(consult-imenu :wk "Imenu")
      "l" '(link-hint-open-link :wk "Link")
      "o" '(consult-outline :wk "Outline")
      "m" '(consult-mark :wk "Mark")
      "M" '(consult-global-mark :wk "Global Mark")
      "n" '(next-error :wk "Next Error")
      "p" '(previous-error :wk "Prev Error")
      "[" '(diff-hl-previous-hunk :wk "Previous Hunk")
      "]" '(diff-hl-next-hunk :wk "Next Hunk")
      )

    ;; clean some global key bindings
    (keymap-global-unset "M-g TAB")
    (keymap-global-unset "M-g M-g")

    ;; M-. M-, M-? for lsp-bridge set in init-lsp-bridge :config

    (agent-shell-leader-def
     "" '(:ignore t :wk "Agent")
     ;; Main agent controls
     "=" '(agent-shell :wk "Start/Reuse Agent Shell")
     "1" '(riven/start-claude-code :wk "Start Claude Code")
     "2" '(riven/start-open-code :wk "Start Open Code")
     "3" '(riven/start-cursor-acp :wk "Start Cursor ACP"))

    ;; Global agent shell keybindings
    ;; If agent-shell not started, start it with C-c =; otherwise show transient menu
    (defun riven/agent-shell-dispatch ()
      "Dispatch agent-shell: start if not running, show transient if running."
      (interactive)
      (if (and (fboundp 'agent-shell-buffers)
               (agent-shell-buffers))
          (when (fboundp 'agent-shell-transient)
            (agent-shell-transient))
        (call-interactively 'agent-shell)))
    (keymap-global-set "M-*" #'riven/agent-shell-dispatch)))

(add-hook 'after-init-hook #'keybindings-config)

;; LSP-bridge keybindings: set up when lsp-bridge loads (may be deferred)
(defun riven/lsp-bridge-keybindings ()
  "Set up lsp-bridge mode keybindings. Called when lsp-bridge loads."
  (when (featurep 'lsp-bridge)
    (leader-def :keymaps 'lsp-bridge-mode-map
      :infix "c"
      "" '(:ignore t :wk "Code")
      "a"  '(lsp-bridge-code-action :wk "Code actions")
      "e"  '(lsp-bridge-diagnostic-list :wk "Diagnostic list")
      "d" '(lsp-bridge-find-def-ex :wk "Find define")
      "f" '(lsp-bridge-code-format :wk "Format code")
      "i"  '(lsp-bridge-find-impl :wk "Find implementation")
      "k"  '(lsp-bridge-popup-documentation :wk "Find Document")
      "p"  '(lsp-bridge-peek :wk "Peek")
      "q" '(eslint-fix :wk "QuickFix (Eslint)")
      "r" '(lsp-bridge-rename :wk "Rename")
      "t"  '(lsp-bridge-find-type-def :wk "Find type definition")
      "?" '(lsp-bridge-find-references :wk "Find References"))))
(with-eval-after-load 'lsp-bridge
  (riven/lsp-bridge-keybindings))

(provide 'init-keybindings)

;;; init-keybindings.el ends here
