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
      "n" '(next-buffer :wk "Next buffer")
      "q" '(read-only-mode :wk "ReadOnly buffer"))

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
      "d" '(crux-recentf-find-directory :wk "Find recent directory")
      "f" '(consult-fd :wk "Find file in directory")
      "l" '(+consult-fd-in-home :wk "Locate file")
      "r" '(consult-recent-file :wk "Recent files")
      "s" '(blink-search :wk "Search files"))

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

    (lookup-leader-def
      "" '(:ignore t :wk "Lookup")
      "d" '(devdocs-lookup :wk "DevDocs")
      "." '(fanyi-dwim :wk "Dictionary")
      "m" '(woman :wk "Unix Manual")
      "g" '(riven/google-search :wk "Google")
      "t" '(riven/google-translate :wk "Google Translator"))

    (gpt-leader-def
      "" '(:ignore t :wk "GPT")
      "a" '(gptel-aibo :wk "Aibo Console")
      "g" '(gptel :wk "GPT Console")
      "s" '(gptel-send :wk "Send region or text to point")
      "r" '(gptel-rewrite :wk "Rewrite(Refactor)")
      "t" '(gptel-translate-to-english-and-replace :wk "Translate to English in place")
      "T" '(gptel-translate-to-langs :wk "Translate to many languages")
      "?" '(gptel-extensions-ask-document :wk "Ask document")
      "TAB" '(gptel-aibo-complete-at-point :wk "Completion at point")
      "<return>" '(gptel-aibo-apply-last-suggestions :wk "Apply last suggestion"))

    ;; define prefix lable
    (leader-def
      "\!" '(:ignore t :wk "Checker(Flycheck)")
      "&" '(:ignore t :wk "Yasnippet")
      "@" '(:ignore t :wk "Hideshow"))
    ))


(add-hook 'after-init-hook #'keybindings-config)

(provide 'init-keybindings)
