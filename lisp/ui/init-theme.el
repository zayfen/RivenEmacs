;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-theme.el --- config theme

;; Functions provided by packages that are only loaded at runtime; declare
;; them so byte-compiling this file stays warning-free.
(declare-function ef-themes-get-color-value "ef-themes" (color &optional theme))
(declare-function ligature-set-ligatures "ligature" (characters &rest sequences))

;; use-package with package.el:
;; (use-package dashboard
;;   :vc (:url "https://github.com/emacs-dashboard/emacs-dashboard")
;;   :commands (dashboard-open)
;;   :config
;;   (setq dashboard-banner-logo-title "Welcome to RivenEmacs")
;;   ;; Set the banner
;;   (setq dashboard-startup-banner 'official)
;;   ;; Content is not centered by default. To center, set
;;   (setq dashboard-center-content t)
;;   ;; vertically center content
;;   (setq dashboard-vertically-center-content t)
;;   ;; To disable shortcut "jump" indicators for each section, set
;;   (setq dashboard-show-shortcuts t)
;;   ;; Set projects backend to project-el (built-in) instead of projectile
;;   (setq dashboard-projects-backend 'project-el)
;;   (setq dashboard-items '((recents   . 6)
;;                           (bookmarks . 5)
;;                           (projects  . 5)
;;                           (agenda    . 5)
;;                           (registers . 5)))
;;   (dashboard-setup-startup-hook))

;; Use idle timer to delay dashboard loading for better startup performance
;; Only open dashboard when no files were specified on command line
;; (when (and (= 1 (length command-line-args))  ; Only emacs binary in args
;;            (not (member "-f" command-line-args))  ; Not running function
;;            (not (member "--funcall" command-line-args))  ; Not running function
;;            (not (member "-l" command-line-args))  ; Not loading file
;;            (not (member "--load" command-line-args))  ; Not loading file
;;            (not (member "-e" command-line-args))  ; Not evaluating expression
;;            (not (member "--eval" command-line-args))  ; Not evaluating expression
;;            (not (member "-t" command-line-args))  ; Not running terminal
;;            (not (member "--terminal" command-line-args)))  ; Not running terminal
;;   (run-with-idle-timer rivenEmacs-dashboard-delay nil #'dashboard-open))

(defun riven/theme-warn-missing-nerd-font ()
  "Warn when configured Nerd Font is missing."
  (unless (find-font (font-spec :name nerd-icons-font-family))
    (message "[icons] Nerd Font '%s' not found. Run M-x nerd-icons-install-fonts."
             nerd-icons-font-family)))

;; Shared icon backend for UI modules (modeline, completion icons, etc.).
;; `package-initialize' is called in `early-init.el', so the nerd-icons
;; load-path entry is already present and the manual `directory-files' scan
;; that used to live here is unnecessary.
(use-package nerd-icons
  :if (locate-library "nerd-icons")
  :ensure nil
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  :config
  (riven/theme-warn-missing-nerd-font))

;; load theme and config
;; ef-themes (by Protesilaos) are vivid and legible, and build on top of
;; modus-themes under the hood.  A light theme is loaded by default and
;; `<f5>' toggles between the configured light and dark themes.
(defcustom rivenEmacs-dark-theme 'ef-night
  "Dark Ef theme used by the `<f5>' theme toggle.
Also loaded at startup when `rivenEmacs-default-background' is `dark'.
`ef-night' is a dark sky-blue theme with vivid green, blue and purple
accents.  Other good picks are `ef-cherie' (purple-black with
pink/magenta/gold), `ef-atlantis' (deep indigo with aquatic colors) and
`ef-melissa-dark' (warm honeybee yellow/red/green/cyan).  Use
`M-x ef-themes-select' to preview any of them, or set this variable and
restart Emacs."
  :type '(choice
          (const :tag "ef-night (dark sky blue; green/blue/purple)" ef-night)
          (const :tag "ef-cherie (purple-black; pink/magenta/gold)" ef-cherie)
          (const :tag "ef-atlantis (deep indigo; aquatic colors)" ef-atlantis)
          (const :tag "ef-melissa-dark (warm honeybee colors)" ef-melissa-dark)
          (const :tag "ef-elea-dark (green/brown/magenta)" ef-elea-dark)
          (const :tag "ef-owl (grey; blue/cyan/magenta)" ef-owl)
          (const :tag "ef-dark (black; blue/cyan/magenta)" ef-dark))
  :group 'rivenEmacs)

(defcustom rivenEmacs-light-theme 'ef-deuteranopia-light
  "Light Ef theme loaded at startup by default and toggled to by `<f5>'.
The dark counterpart is `rivenEmacs-dark-theme'.  The default,
`ef-deuteranopia-light', is the accessible light variant with
blue/yellow accents and strong contrast.  Other light alternatives are
`ef-tritanopia-light' (red/pink, tritanopia-safe), `ef-cyprus',
`ef-light', `ef-maris-light', `ef-duo-light', `ef-elea-light',
`ef-melissa-light' and `ef-trio-light'.  Use `M-x ef-themes-select' to
preview them, or set this variable and restart Emacs."
  :type '(choice
          (const :tag "ef-deuteranopia-light (accessible; blue/yellow)"
                 ef-deuteranopia-light)
          (const :tag "ef-tritanopia-light (accessible; red/pink)"
                 ef-tritanopia-light)
          (const :tag "ef-cyprus (light; green/red)" ef-cyprus)
          (const :tag "ef-light (light; blue/cyan/magenta)" ef-light)
          (const :tag "ef-maris-light (light; blue/green)" ef-maris-light)
          (const :tag "ef-duo-light (light; two colors)" ef-duo-light)
          (const :tag "ef-elea-light (light; green/brown)" ef-elea-light)
          (const :tag "ef-melissa-light (light; honeybee colors)"
                 ef-melissa-light)
          (const :tag "ef-trio-light (light; three colors)" ef-trio-light))
  :group 'rivenEmacs)

(defcustom rivenEmacs-default-background 'light
  "Which Ef theme family is loaded at startup.
`light' loads `rivenEmacs-light-theme' (default `ef-deuteranopia-light');
`dark' loads `rivenEmacs-dark-theme' (default `ef-night').  The `<f5>'
toggle switches between the two themes regardless of this setting."
  :type '(choice (const :tag "light (rivenEmacs-light-theme)" light)
                 (const :tag "dark (rivenEmacs-dark-theme)" dark))
  :group 'rivenEmacs)

(use-package ef-themes
  :ensure t
  :demand t                                  ; load now — a theme must be active at startup
  :bind ("<f5>" . ef-themes-toggle)          ; switch between light and dark themes
  :config
  ;; Set theme user options BEFORE loading the theme. These are defcustoms with
  ;; custom :set setters, so use `customize-set-variable' (a plain `setq' or
  ;; `use-package' :custom would not run the setter and the value would be
  ;; silently ignored — verified).
  ;; <f5> toggles between the configured light and dark themes.
  (customize-set-variable
   'ef-themes-to-toggle
   (list rivenEmacs-light-theme rivenEmacs-dark-theme))
  ;; Heading styling: rainbow per-level + weight gradient, no manual foreground
  ;; colors (let the theme decide, so it stays consistent on theme switch).
  ;; Keyed by level (0=document title, 1..6=headings, t=catch-all).
  (customize-set-variable
   'ef-themes-headings
   '((0 . (variable-pitch light 1.4))
     (1 . (rainbow background bold 1.2))
     (2 . (rainbow semibold 1.15))
     (3 . (rainbow semibold 1.1))
     (t . (rainbow semibold))))
  ;; Load the configured light theme by default; see
  ;; `rivenEmacs-default-background'.
  ;; NOTE: `ef-themes-select' is interactive-only (takes no args); to load a
  ;; specific theme programmatically, use `load-theme' directly.
  (load-theme (if (eq rivenEmacs-default-background 'dark)
                  rivenEmacs-dark-theme
                rivenEmacs-light-theme)
              t))


(use-package doom-modeline
  :vc (:url "https://github.com/seagle0128/doom-modeline" :rev "master")
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-major-mode-icon t
        doom-modeline-minor-modes nil
        doom-modeline-buffer-encoding nil
        doom-modeline-word-count nil
        doom-modeline-enable-word-count nil
        doom-modeline-vcs-max-length 24)
  ;; Don’t compact font caches during GC.
  (setq inhibit-compacting-font-caches t))


(defun riven/apply-theme-face-overrides ()
  "Apply one-time face overrides after theme loading."
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil :weight 'semi-bold :slant 'normal)
  (set-face-attribute 'font-lock-function-name-face nil :weight 'semi-bold :slant 'normal)
  (let* ((dark-mode (eq (frame-parameter nil 'background-mode) 'dark))
         ;; Follow the active Ef theme palette so line numbers keep the
         ;; theme's tint instead of a hard-coded grey.
         (theme-color (lambda (name fallback)
                        (or (ignore-errors (ef-themes-get-color-value name))
                            fallback)))
         (line-number-fg (funcall theme-color
                                  'fg-dim
                                  (if dark-mode "#6e7681" "#9aa0a6")))
         (line-number-current-fg (funcall theme-color
                                          'fg-alt
                                          (if dark-mode "#c3ccd7" "#4f5660"))))
    (set-face-attribute 'line-number nil
                        :foreground line-number-fg
                        :background 'unspecified
                        :weight 'normal)
    (set-face-attribute 'line-number-current-line nil
                        :foreground line-number-current-fg
                        :background 'unspecified
                        :weight 'semi-bold)))

(defun riven/reapply-theme-face-overrides (&rest _)
  "Reapply face overrides after theme changes."
  (riven/apply-theme-face-overrides))

(riven/apply-theme-face-overrides)

(unless (advice-member-p #'riven/reapply-theme-face-overrides 'load-theme)
  (advice-add 'load-theme :after #'riven/reapply-theme-face-overrides))


(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))



;; This assumes you've installed the package via MELPA.
(use-package ligature
  :vc (:url "https://github.com/mickeynp/ligature.el" :rev "master")
  :hook (prog-mode . ligature-mode)
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures t '("www"))
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

(defun rivenEmacs-enable-fancy-compilation-mode ()
  "Enable `fancy-compilation-mode' when its package is available."
  (when (fboundp 'fancy-compilation-mode)
    (fancy-compilation-mode 1)))

(with-eval-after-load 'compile
  (rivenEmacs-enable-fancy-compilation-mode))

;; Show file path in header line (first line)
(setq-default header-line-format
              '((:propertize (:eval (when buffer-file-name
                                      (abbreviate-file-name buffer-file-name)))
                 face (:height 0.9 :foreground "gray60"))))

(provide 'init-theme)
;;; init-theme.el ends here
