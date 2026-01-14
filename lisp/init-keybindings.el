;; -*- coding: utf-8; lexical-binding: t -*-

;;; Code:
;; config project keybindings

(defun keybindings-config()
   (progn
     ;; Ensure agent-shell functions are available before defining keybindings
     (when (featurep 'agent-shell)
       ;; Load agent-shell functions if available
       (with-eval-after-load 'agent-shell
         ;; Functions are already defined in init-gpt.el via with-eval-after-load
         nil))
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
      "?" '(lsp-bridge-find-references :wk "Find References"))

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
       "c" '(gptel :wk "Console")
       "g" '(gptel-menu :wk "Gptel Menu")
       "s" '(gptel-send :wk "Send")
       
       ;; 代码理解
       "e" '(gptel-explain-code :wk "Explain Code")
       "?" '(gptel-ask-about-code :wk "Ask About Code")
       "d" '(gptel-generate-docstring :wk "Generate Docstring")
       
       ;; 代码改进
       "r" '(gptel-rewrite :wk "Rewrite")
       "o" '(gptel-optimize-code :wk "Optimize Code")
       "f" '(gptel-fix-code :wk "Fix Code")
       "m" '(gptel-simplify-code :wk "Simplify Code")
       "R" '(gptel-suggest-refactor :wk "Refactor Suggestions")
       
       ;; 代码生成
       "a" '(gptel-add-comments :wk "Add Comments")
       "t" '(gptel-generate-test :wk "Generate Test")
       "C" '(gptel-complete-code :wk "Complete Code")
       
       ;; 代码质量
       "v" '(gptel-review-code :wk "Review Code")
       "S" '(gptel-check-security :wk "Check Security")
       
       ;; 其他功能
       "T" '(gptel-translate-region :wk "Translate")
       "y" '(gptel-convert-style :wk "Convert Style")
       "E" '(gptel-explain-error :wk "Explain Error")
       "D" '(gptel-extensions-ask-document :wk "Ask Document"))

     ;; Agent Shell keybindings - delayed until agent-shell is loaded
     ;; Note: using with-eval-after-load because agent-shell is lazy-loaded by use-package

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

    ;; set other global key bindings
    (when (featurep 'lsp-bridge)
      (keymap-global-set "M-." #'lsp-bridge-find-def)
      (keymap-global-set "C-," #'lsp-bridge-find-def-return))

    ))

(add-hook 'after-init-hook #'keybindings-config)

;; Agent Shell keybindings - loaded after agent-shell is available
(general-create-definer agent-shell-leader-def
  :prefix "C-c =")

(agent-shell-leader-def
  "" '(:ignore t :wk "Agent")
  ;; Main agent controls
  "=" '(agent-shell :wk "Start/Reuse Agent Shell")
  "n" '(agent-shell-new-shell :wk "New Agent Shell")
  "t" '(agent-shell-toggle :wk "Toggle Agent Shell")
  "c" '(riven/start-claude-code :wk "Start Claude Code")
  "u" '(riven/start-cursor-acp :wk "Start Cursor ACP")
  "o" '(riven/start-open-code :wk "Start Open Code")
  "+" '(agent-shell-transient :wk "Agent Menu")
  "d" '(riven/agent-shell-diagnose :wk "Diagnose Agents")
  ;; Code operations
  "r" '(riven/agent-shell-code-review :wk "Review Code")
  "e" '(riven/agent-shell-explain-code :wk "Explain Code")
  "f" '(riven/agent-shell-refactor-code :wk "Refactor Code")
  "T" '(riven/agent-shell-add-tests :wk "Add Tests")
  "s" '(riven/agent-shell-send-current-function :wk "Send Function")
  ;; Context and analysis
  "b" '(riven/agent-shell-send-buffer-context :wk "Buffer Context")
  "p" '(riven/agent-shell-send-project-context :wk "Project Context")
  "A" '(riven/agent-shell-analyze-project :wk "Analyze Project")
  "/" '(riven/agent-shell-search-codebase :wk "Search Codebase")
  "g" '(riven/agent-shell-git-status :wk "Git Status")
  ;; Documentation and quality
  "D" '(riven/agent-shell-generate-docs :wk "Generate Docs")
  "G" '(riven/agent-shell-generate-tests :wk "Generate Tests")
  "L" '(riven/agent-shell-lint-code :wk "Lint Code"))

;; Global agent shell keybindings
(keymap-global-set "M-+" #'agent-shell-transient)

(provide 'init-keybindings)
