;;; -*- coding: utf-8; lexical-binding: t -*-

(defun +directory-ensure (&rest path-parts)
  "Concatenate PATH-PARTS to construct a path and return it.

Ensure the path exists, if not create it. The exact behavior is to create the
parent directory if the path is a file, and if the path is a directory, create
that directory."
  (let* ((path (mapconcat #'identity path-parts nil))
         (parent-dir (file-name-directory path)))
    (unless (file-directory-p parent-dir)
      (ignore-errors (mkdir parent-dir t))
      (unless (file-directory-p parent-dir)
        (+error! "Cannot create directory %s" parent-dir)))
    path))

(setq
 gc-cons-threshold 100000000
 max-specpdl-size 5000

 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil

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
 line-spacing 0.2

 ;; ====== Recent files ======
 ;; Increase the maximum number of saved items
 recentf-max-saved-items 50
 ;; Ignore case when searching recentf files
 recentf-case-fold-search t
 ;; Exclude some files from being remembered by recentf
 recentf-exclude
 `(,(rx (* any)
        (or
         "elfeed-db"
         "eln-cache"
         "/cache/"
         ".maildir/"
         ".cache/"
         "node_modules"
         ".git")
        (* any)
        (? (or "html" "pdf" "tex" "epub")))
   ,(rx "/"
        (or "rsync" "ssh" "tmp" "yadm" "sudoedit" "sudo")
        (* any)))

 ;; ====== Default directories for builtin packages ======
 auto-save-list-file-prefix (+directory-ensure local-dir "auto-save/")
 abbrev-file-name (concat local-dir "abbrev.el")
 project-list-file (concat local-dir "project-list.el")
 url-configuration-directory (+directory-ensure local-dir "url/")
 url-cookie-file (concat local-dir "url/cookie.el")
 url-history-file (concat local-dir "url/history.el")
 url-cache-directory (+directory-ensure cache-dir "url/")
 save-place-file (concat local-dir "save-place.el")
 savehist-file (concat local-dir "savehist.el")
 org-id-locations-file (concat cache-dir "org/id-locations.el")
 org-persist-directory (+directory-ensure cache-dir "org/persist/")
 org-publish-timestamp-directory (+directory-ensure cache-dir "org/publish/timestamps/")
 org-preview-latex-image-directory (+directory-ensure cache-dir "org/preview/latex-image/")
 recentf-save-file (concat local-dir "recentf-save.el")
 type-break-file-name (concat local-dir "type-break.el")
 bookmark-default-file (concat local-dir "bookmark.el")
 ede-project-placeholder-cache-file (concat local-dir "ede-projects.el")
 kkc-init-file-name (concat local-dir "kkc-init-file.el")
 eshell-directory-name (+directory-ensure local-dir "eshell/")
 eshell-history-file-name (concat local-dir "eshell/history.el")
 eshell-last-dir-ring-file-name (concat local-dir "eshell/last-dir-ring.el")
 eshell-aliases-file (concat local-dir "eshell/aliases")
 eshell-rc-script (concat local-dir "eshell/rc")
 eshell-login-script (concat local-dir "eshell/login")
 calc-settings-file (concat local-dir "calc-settings.el")
 auto-insert-directory (+directory-ensure local-dir "auto-insert/")
 image-dired-dir (+directory-ensure local-dir "image-dired/")
 image-dired-tags-db-file (concat local-dir "image-dired/tags-db.el")
 image-dired-temp-rotate-image-file (concat cache-dir "image-dired/temp-rotate-image")
 eudc-options-file (concat local-dir "eudc-options.el")
 eww-bookmarks-directory (+directory-ensure local-dir "eww/bookmarks/")
 shadow-info-file (concat local-dir "shadow/info.el")
 shadow-todo-file (concat local-dir "shadow/todo.el")
 semanticdb-default-system-save-directory (concat local-dir "semantic/")
 desktop-dirname (+directory-ensure local-dir "desktop/")
 desktop-path (list desktop-dirname))

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default
 indent-tabs-mode nil
 truncate-lines nil
 fill-column 120
 display-line-numbers-width 4
 tab-width 2)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(delete-selection-mode t)
;;(global-display-line-numbers-mode t)
(column-number-mode)
(savehist-mode)

(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)


;; Hide/show code blocks, a.k.a. code folding
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'conf-mode-hook #'hs-minor-mode)


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
  (pixel-scroll-mode 1))

(display-time-mode 1)

(when (eq system-type 'darwin)
  (setq ns-auto-hide-menu-bar t))

(add-hook 'compilation-mode-hook 'visual-line-mode)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

(recentf-mode 1)
(desktop-save-mode 1)
(global-subword-mode 1)



(provide 'init-default)
