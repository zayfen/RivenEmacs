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
      "o" '(blink-search :wk "Occur"))

    (leader-def :infix "g"
      "" '(:ignore t :wk "Git")
      "b" '(magit-blame :wk "Blame")
      "d" '(magit-diff-dwim :wk "Diff")
      "c" '(magit-clone :wk "Clone")
      "g" '(magit-status :wk "Status")
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
      "r" '(vr/replace :wk "Replace")
      "s" '(sp-delete-sexp :wk "Delete Sexp")
      "z" '(zap-to-char :wk "Zap to char"))

    (open-leader-def
      "" '(:ignore t :wk "Open")
      "d" '(docker :wk "Docker")
      "e" '(+open-in-system-explorer :wk "Explorer")
      "n" '(elfeed :wk "News")
      "q" '(quickrun :wk "Quickrun")
      "t" '(+open-term-in-current-directory :wk "Terminal"))

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
      "s" '(gptel-send :wk "Send")
      "r" '(gptel-rewrite :wk "Rewrite(Refactor)")
      "t" '(gptel-translate-region :wk "Translate via AI")
      "?" '(gptel-extensions-ask-document :wk "Ask document"))

    ;; define prefix lable
    (leader-def
      "\!" '(:ignore t :wk "Checker(Flycheck)")
      "&" '(:ignore t :wk "Yasnippet")
      "@" '(:ignore t :wk "Hideshow"))

    ;; define navigate to elements keybindings
    (navigate-leader-def
     "c" '(avy-goto-char-2 :wk "Goto Char")
     "g" '(consult-goto-line :wk "Line")
     "i" '(consult-imenu-multi :wk "Imenu")
     "l" '(link-hint-open-link :wk "Link")
     "o" '(consult-outline :wk "Outline")
     "m" '(consult-mark :wk "Mark")
     "M" '(consult-global-mark :wk "Global Mark")
     "n" '(next-error :wk "Next Error")
     "p" '(previous-error :wk "Prev Error")
     )
    ))

(add-hook 'after-init-hook #'keybindings-config)

(provide 'init-keybindings)
