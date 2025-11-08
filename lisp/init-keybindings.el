;; -*- coding: utf-8; lexical-binding: t -*-

;;; Code:
;; config project keybindings

(defun keybindings-config()
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
      "" '(:ignroe t :wk "Note (Org)")
      "a" '(org-agenda-list :wk "Agenda List")
      "ci" '(org-agenda-clock-in :wk "Clock in")
      "co" '(org-agenda-clock-out :wk "Clock out")
      "cg" '(org-agenda-clock-goto :wk "Clock goto current")
      "s" '(org-ql-search :wk "Search")
      "w" '(report-last-week-tasks :wk "Last Week Tasks"))

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
      "a" '(aidermacs-transient-menu :wk "Aider")
      "c" '(gptel :wk "Console")
      "g" '(gptel-menu :wk "Gptel Menu")
      "s" '(gptel-send :wk "Send")
      "r" '(gptel-rewrite :wk "Rewrite(Refactor)")
      "t" '(gptel-translate-region :wk "Translate via AI")
      "?" '(gptel-extensions-ask-document :wk "Ask document"))

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

(provide 'init-keybindings)
