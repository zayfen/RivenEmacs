;; -*- coding: utf-8; lexical-binding: t -*-

(defun get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

;; Ripgrep the current word from project root
(defun consult-ripgrep-at-point ()
  (interactive)
  (consult-ripgrep (get-project-root) (thing-at-point 'symbol)))

;; Ripgrep the selected region from project root
(defun consult-ripgrep-region ()
  (interactive)
  (consult-ripgrep (get-project-root) (buffer-substring-no-properties (region-beginning) (region-end))))


;; config project keybindings
(leader-def
  :infix "p"
  "c" '(project-forget-zombie-projects :wk "Forget zombie projects")
  "f" '(project-find-file :wk "Find file in project")
  "p" '(project-switch-project :wk "Switch project")
  "s" '(consult-ripgrep :wk "Search symbol")
  "S" '(consult-ripgrep-region :wk "Search region")
  "b" '(consult-project-buffer :wk "Buffers in project")
  "/" '(consult-git-grep :wk "Git grep")
  "r" '(color-rg-search-symbol-in-project :wk "Search/Replace")
  "." '(consult-ripgrep-at-point :wk "Search symbol at point"))

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



(provide 'init-keybindings)
