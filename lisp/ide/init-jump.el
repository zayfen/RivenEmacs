;; -*- coding: utf-8; lexical-binding: t; -*-

;; install switch-window
(use-package switch-window
  :custom
  ;; Use the home-row QWERTY keys (a/s/d/f/...) as window labels so the
  ;; picker is one keystroke per window under the home row.
  (switch-window-shortcut-style 'qwerty)
  ;; Keep the labels short (a/s/d...) instead of drawing big ASCII boxes.
  (switch-window-shortcut-appearance 'text)
  ;; Pop to the minibuffer when picking, with `z' as its shortcut.
  (switch-window-input-style 'minibuffer)
  (switch-window-minibuffer-shortcut ?z)
  ;; Only show the picker when there are at least 2 windows (no flicker for 1).
  (switch-window-threshold 2)
  ;; Auto-fit the chosen window to ~70% of the frame after maximizing.
  (switch-window-auto-resize-window nil)
  (switch-window-default-window-size 0.7)
  :bind
  ("C-x o"   . switch-window)
  ("C-x 1"   . switch-window-then-maximize)
  ("C-x 2"   . switch-window-then-split-below)
  ("C-x 3"   . switch-window-then-split-right)
  ("C-x 0"   . switch-window-then-delete)
  ("C-x 4 d"   . switch-window-then-dired)
  ("C-x 4 f"   . switch-window-then-find-file)
  ("C-x 4 m"   . switch-window-then-compose-mail)
  ("C-x 4 r"   . switch-window-then-find-file-read-only)
  ("C-x 4 C-f" . switch-window-then-find-file)
  ("C-x 4 C-o" . switch-window-then-display-buffer)
  ("C-x 4 0"   . switch-window-then-kill-buffer))

(set-frame-parameter (selected-frame)
                     'internal-border-width 0)


(use-package goto-last-change
  :bind ("M-[" . goto-last-change)
  :commands goto-last-change)


(use-package dumb-jump
  :vc (:url "https://github.com/jacktasia/dumb-jump")
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate -100)
  :after xref
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-disable-obsolete-warnings t))

(provide 'init-jump)
