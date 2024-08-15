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

(use-package doom-modeline
  :vc (:fetcher github :repo seagle0128/doom-modeline)
  :hook (after-init . doom-modeline-mode))


;; This assumes you've installed the package via MELPA.
(use-package ligature
  :vc (:fetcher github :repo mickeynp/ligature.el)
  :init (global-ligature-mode t)
  :hook ((prog-mode . ligature-mode))
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
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
                                       "\\\\" "://"))
  )

(use-package composite
  :ensure nil
  :defer t
  :hook (prog-mode . auto-composition-mode)
  :init (global-auto-composition-mode -1))


(provide 'init-theme)
