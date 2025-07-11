;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-theme.el --- config theme

;; use-package with package.el:
(use-package dashboard
  :vc (:url "https://github.com/emacs-dashboard/emacs-dashboard")
  :commands (dashboard-open)
  :config
  (setq dashboard-banner-logo-title "Welcome to RivenEmacs")
  ;; Set the banner
  (setq dashboard-startup-banner 'official)
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  ;; vertically center content
  (setq dashboard-vertically-center-content t)
  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts t)
  (setq dashboard-items '((recents   . 6)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)
                          (registers . 5)))
  (dashboard-setup-startup-hook))

;; Use idle timer to delay dashboard loading for better startup performance
(run-with-idle-timer rivenEmacs-dashboard-delay nil #'dashboard-open)

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



(use-package doom-modeline
  :vc (:url "https://github.com/seagle0128/doom-modeline")
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  ;; Don’t compact font caches during GC.
  (setq inhibit-compacting-font-caches t))


;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (progn
;;               ;; Customize active mode-line
;;               (set-face-attribute 'mode-line nil
;;                                   :background "#1a1a1a"
;;                                   :font (font-spec :weight 'semi-bold))

;;               ;; Customize inactive mode-line
;;               (set-face-attribute 'mode-line-inactive nil
;;                                   :background "#2a2a2a"
;;                                   :font (font-spec :weight 'regular)))))

;;;;;;;; customize mode-line end ;;;;;;;;;;;;;;


(add-hook 'prog-mode-hook (lambda ()
                            ;; (set-face-attribute 'fringe nil :background "#000000") ;; setting for modus-vivendi theme
                            (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
                            (set-face-attribute 'font-lock-keyword-face nil :weight 'semi-bold :slant 'italic)
                            (set-face-attribute 'font-lock-function-name-face nil :weight 'semi-bold :slant 'italic)))


(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))



;; This assumes you've installed the package via MELPA.
(use-package ligature
  :vc (:url "https://github.com/mickeynp/ligature.el")
  :hook ((prog-mode . ligature-mode))
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures t '("www"))
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


(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  ;; :hook after-init
  :hook (after-init . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '( :internal-border-width 8
      :header-line-width 4
      :mode-line-width 2
      :tab-width 4
      :right-divider-width 2
      :scroll-bar-width 4
      :fringe-width 8)))


;; beautiful compilation buffer
(use-package fancy-compilation
  :commands (fancy-compilation-mode))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

;; change line number color
;; (set-face-foreground 'line-number "#808080")

(provide 'init-theme)
;;; init-theme.el ends here
