;; -*- coding: utf-8; lexical-binding: t -*-

;; config project keybindings
(leader-def
  :infix "p"
  "c" '(project-forget-zombie-projects :wk "Forget zombie projects")
  "f" '(project-find-file :wk "Find file in project")
  "g" '(+goto-file-at-point :wk "Goto file at point")
  "p" '(project-switch-project :wk "Switch project")
  "s" '(consult-ripgrep-ex :wk "Search symbol")
  "b" '(consult-project-buffer :wk "Buffers in project")
  "/" '(consult-git-grep :wk "Git grep")
  "r" '(color-rg-search-symbol-in-project :wk "Search/Replace"))

(defun +consult-fd-in-home ()
  (interactive)
  "Find any file from home directory"
  (consult-fd "~/"))

(leader-def
  :infix "f"
  "d" '(crux-recentf-find-directory :wk "Find recent directory")
  "f" '(consult-fd :wk "Find file in directory")
  "l" '(+consult-fd-in-home :wk "Locate file")
  "r" '(consult-recent-file :wk "Recent files")
  "s" '(blink-search :wk "Search files"))

(leader-def
  "l" '(devdocs-lookup :wk "Lookup DevDocs"))



(provide 'init-keybindings)
