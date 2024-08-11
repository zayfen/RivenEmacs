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

;; enhance ripgrep
(defun consult-ripgrep-ex ()
  (interactive)
  (if (use-region-p)
      (consult-ripgrep-region)
    (consult-ripgrep-at-point)))

(defun my-select-inside-quotes ()
  "grab text between double straight quotes on each side of cursor."
  (interactive)
  (let (p1 p2)
    (skip-chars-backward "^[\"\']" (line-beginning-position))
    (setq p1 (point))
    (skip-chars-forward "^[\"\']" (line-end-position))
    (setq p2 (point))
    (buffer-substring-no-properties p1 p2)))


(defun +goto-file-at-point ()
  "Find the file at point and open it."
  (interactive)
  (let (file-path)
    (setq file-path (my-select-inside-quotes))
    (consult-fd (get-project-root) file-path)
    ))

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
  "e" '(dirvish-side :wk "Explorer"))



(provide 'init-keybindings)
