;;; -*- coding: utf-8; lexical-binding: t -*-


(setq gc-cons-threshold 100000000)
(setq max-specpdl-size 5000)

(setq
 ;; No need to see GNU agitprop.
 inhibit-startup-screen t
 ;; No need to remind me what a scratch buffer is.
 initial-scratch-message nil
 ;; Double-spaces after periods is morally wrong.
 sentence-end-double-space nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t
 ;; Prompts should go in the minibuffer, not in a GUI.
 use-dialog-box nil
 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive nil
 ;; Let C-k delete the whole line.
 kill-whole-line t
 ;; search should be case-sensitive by default
 case-fold-search nil
 ;; accept 'y' or 'n' instead of yes/no
 ;; the documentation advises against setting this variable
 ;; the documentation can get bent imo
 use-short-answers t
 ;; my source directory
 default-directory "~/Github/"
 ;; eke out a little more scrolling performance
 fast-but-imprecise-scrolling t
 ;; prefer newer elisp files
 load-prefer-newer t
 ;; when I say to quit, I mean quit
 confirm-kill-processes nil
 ;; if native-comp is having trouble, there's not very much I can do
 native-comp-async-report-warnings-errors 'silent
 ;; unicode ellipses are better
 truncate-string-ellipsis "…"
 ;; I want to close these fast, so switch to it so I can just hit 'q'
 help-window-select t
 ;; this certainly can't hurt anything
 delete-by-moving-to-trash t
 ;; keep the point in the same place while scrolling
 scroll-preserve-screen-position t
 ;; more info in completions
 completions-detailed t
 ;; highlight error messages more aggressively
 next-error-message-highlight t
 ;; don't let the minibuffer muck up my window tiling
 read-minibuffer-restore-windows t
 ;; scope save prompts to individual projects
 save-some-buffers-default-predicate 'save-some-buffers-root
 ;; don't keep duplicate entries in kill ring
 kill-do-not-save-duplicates t
 desktop-save-buffer t
 line-spacing 0.15
 )

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(delete-selection-mode t)
(global-display-line-numbers-mode t)
(column-number-mode)
(savehist-mode)

(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)


;; (defun zyf/unbind-bad-keybindings ()
;;   "Remove unhelpful keybindings."
;;   (map (lambda (x) (unbind-key x)) '("C-x C-f" ;; find-file-read-only
;;                                       "C-x C-d" ;; list-directory
;;                                       "C-z" ;; suspend-frame
;;                                       "C-x C-z" ;; again
;;                                       "<mouse-2>" ;; pasting with mouse-wheel click
;;                                       "<C-wheel-down>" ;; text scale adjust
;;                                       "<C-wheel-up>" ;; ditto
;;                                       "s-n" ;; make-frame
;;                                       "s-t" ;; ns-popup-font-panel
;;                                       "s-p" ;; ns-print-buffer
;;                                       "C-x C-q" ;; read-only-mode
;;                                       )))

;; (zyf/unbind-bad-keybindings)

(bind-key "s-<up>" #'ff-find-related-file)
(bind-key "C-c a f" #'ff-find-related-file)
(bind-key "C-s" #'isearch-forward-regexp)

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)


(use-package dabbrev
;;  :bind* (("M-/" . #'dabbrev-completion))
  :custom
  (dabbrev-check-all-buffers t)
  (dabbrev-case-replace nil))

(add-to-list 'default-frame-alist '(fullscreen . maximized))


(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (pixel-scroll-mode))

(when (eq system-type 'darwin)
  (setq ns-auto-hide-menu-bar t))

(add-hook 'compilation-mode-hook 'visual-line-mode)

(setq-default fill-column 120)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

(recentf-mode)

(use-package hl-todo
  :vc (:fetcher github :repo tarsius/hl-todo)
  :config
  (hl-todo-mode))


(provide 'init-default)
