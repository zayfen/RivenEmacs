;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-theme.el --- config theme

;; use-package with package.el:
(use-package dashboard
  :vc (:fetcher github :repo emacs-dashboard/emacs-dashboard)
  :commands (dashboard-open)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to RivenEmacs")
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
                          (registers . 5))))

(add-hook 'after-init-hook (lambda () (dashboard-open)))


;; load theme and config
;; (load-theme 'modus-vivendi t)
;; (load-theme 'modus-operandi-tritanopia t)
(load-theme 'modus-vivendi-tritanopia t)

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

(defun custom-buffer-name ()
  "Return the buffer name, prepending the directory name if the file is named 'index' (ignoring extension)."
  (let* ((name (buffer-name))
         (file (buffer-file-name))
         (filename (when file (file-name-nondirectory file)))
         (basename (when filename (file-name-base filename)))
         (dirname (when file (file-name-nondirectory (directory-file-name (file-name-directory file))))))
    (if (and basename (string= basename "index"))
        (concat dirname "/" name)
      name)))

(setq-default mode-line-buffer-identification
              '(:eval (custom-buffer-name)))

;; show project name on modeline
(setq-default mode-line-format
              (cons '(:eval (when-let ((project (project-current))
                                       (project-root (project-root project)))
                              (format " [%s]" (file-name-nondirectory (directory-file-name project-root)))))
                    mode-line-format))

(add-hook 'prog-mode-hook (lambda ()
                            ;; (set-face-attribute 'fringe nil :background "#000000") ;; setting for modus-vivendi theme
                            (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
                            (set-face-attribute 'font-lock-keyword-face nil :weight 'bold :slant 'italic)
                            (set-face-attribute 'font-lock-function-name-face nil :weight 'bold :slant 'italic)))


(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; (use-package doom-modeline
;;   :vc (:fetcher github :repo seagle0128/doom-modeline)
;;   :hook (after-init . doom-modeline-mode))


;; This assumes you've installed the package via MELPA.
(use-package ligature
  :vc (:fetcher github :repo mickeynp/ligature.el)
  :hook ((prog-mode . ligature-mode))
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (global-ligature-mode t)
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://")))


(use-package minions
  :vc (:fetcher github :repo tarsius/minions)
  :config
  (setq minions-mode-line-lighter "👋")
  (add-to-list 'minions-prominent-modes 'flycheck-mode)
  (minions-mode 1))
(add-hook 'after-init-hook 'minions-mode)

(use-package spacious-padding
  :init
  (spacious-padding-mode 1))

(provide 'init-theme)
