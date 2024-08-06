;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-theme.el --- config theme

;; use-package with package.el:
(use-package dashboard
  :vc (:fetcher github :repo emacs-dashboard/emacs-dashboard)
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;; Set the banner
  (setq dashboard-startup-banner 'official)
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  ;; vertically center content
  (setq dashboard-vertically-center-content t)
  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)

  (setq dashboard-items '((recents   . 5)
                        (bookmarks . 5)
                        (projects  . 5)
                        (agenda    . 5)
                        (registers . 5)))

  (dashboard-setup-startup-hook))

(dashboard-open)


;; load theme and config
(load-theme 'modus-vivendi t)
(setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-custom-auto-reload t
        modus-themes-headings
        '(
          (underline-link border)
          (underline-link-visited border)
          (underline-link-symbolic border)))

(add-hook 'prog-mode-hook (lambda ()
                            (set-face-attribute 'fringe nil :background "#000000")
                            (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
                            (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
                            (set-face-attribute 'font-lock-function-name-face nil :slant 'italic)))


(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))


(defun pt/project-relative-file-name (include-prefix)
  "Return the project-relative filename, or the full path if INCLUDE-PREFIX is t."
  (letrec
      ((fullname (if (equal major-mode 'dired-mode) default-directory (buffer-file-name)))
       (root (project-root (project-current)))
       (relname (if fullname (file-relative-name fullname root) fullname))
       (should-strip (and root (not include-prefix))))
    (if should-strip relname fullname)))

(use-package mood-line
  :config
  (defun pt/mood-line-segment-project-advice (oldfun)
    "Advice to use project-relative file names where possible."
    (let
        ((project-relative (ignore-errors (pt/project-relative-file-name nil))))
         (if
             (and (project-current) (not org-src-mode) project-relative)
             (propertize (format "%s  " project-relative) 'face 'mood-line-buffer-name)
           (funcall oldfun))))

  (advice-add 'mood-line-segment-buffer-name :around #'pt/mood-line-segment-project-advice)
  (mood-line-mode))


(use-package doom-modeline
  :ensure t
  :vc (:fetcher github :repo seagle0128/doom-modeline)
  :hook (after-init . doom-modeline-mode))


(provide 'init-theme)
