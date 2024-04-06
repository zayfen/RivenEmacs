;; re-core-ui.el --- RivenEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


;; (defun +theme--tweaks-h (&optional _)
;;   "Use smaller font (75% of the default) for line numbers in graphic mode."
;;   (when (display-graphic-p)
;;     (set-face-attribute
;;      'line-number nil
;;      :background (face-attribute 'default :background)
;;      :height (truncate (* 0.95 (face-attribute 'default :height)))
;;      :weight 'semibold)
;;     (set-face-attribute
;;      'line-number-current-line nil
;;      :height (truncate (* 0.95 (face-attribute 'default :height)))
;;      :weight 'bold)))

;; Apply tweaks
;; (add-hook 'after-init-hook #'+theme--tweaks-h)
;; (add-hook 'enable-theme-functions #'+theme--tweaks-h)

;; Save enabled theme
(add-hook
 'enable-theme-functions
 (defun +theme--save-enabled-theme-h (theme)
   "Save the enabled theme to `rivenemacs-theme'.
Useful for keeping track of the enabled theme."
   (setq rivenemacs-theme theme)))

;; Disable previously enabled custom themes before enabling a new one.
(advice-add
 'load-theme :before
 (defun +theme--disable-previous-themes-a (&rest _)
   "Disable previously enabled themes before enabling the new one."
   (mapc #'disable-theme custom-enabled-themes)))

(use-package modus-themes
  :straight (:host github :repo "protesilaos/modus-themes")
  :config
  ;; In all of the following, WEIGHT is a symbol such as `semibold',
  ;; `light', `bold', or anything mentioned in `modus-themes-weights'.
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-custom-auto-reload t
        modus-themes-headings
        '(
          (underline-link border)
          (underline-link-visited border)
          (underline-link-symbolic border))))

(use-package all-the-icons
  :straight t
  :config
  ;; Show .m files as matlab/octave files
  (setcdr (assoc "m" all-the-icons-extension-icon-alist)
          (cdr (assoc "matlab" all-the-icons-extension-icon-alist))))


(use-package dashboard
  :straight t
  :demand t
  :init
  (+map! "oD" #'dashboard-open)
  :custom
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-banner-ascii "RivenEmacs")
  (dashboard-banner-logo-title "Welcome to RivenEmacs!")
  (dashboard-items '((recents . 5) (projects . 5) (bookmarks . 5)))
  (dashboard-image-banner-max-width 600)
  (dashboard-projects-backend 'project-el)
  :config
  ;; Ensure setting the keybindings before openning the dashboard
  ;; (evil-collection-dashboard-setup)
  (dashboard-open))

(use-package doom-modeline
  :straight t
  :hook (rivenemacs-after-startup . doom-modeline-mode)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 8)
  (doom-modeline-time-icon t)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-icon t)
  ;; Don’t compact font caches during GC.
  (inhibit-compacting-font-caches t))


(provide 're-core-ui)
