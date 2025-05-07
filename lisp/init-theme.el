;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-theme.el --- config theme

;; use-package with package.el:
(use-package dashboard
  :vc (:fetcher github :repo emacs-dashboard/emacs-dashboard)
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

;; customize mode-line
;; (defun custom-buffer-name ()
;;   "Return the buffer name, prepending the directory name if the file is named 'index' (ignoring extension)."
;;   (let* ((name (buffer-name))
;;          (file (buffer-file-name))
;;          (filename (when file (file-name-nondirectory file)))
;;          (basename (when filename (file-name-base filename)))
;;          (dirname (when file (file-name-nondirectory (directory-file-name (file-name-directory file))))))
;;     (if (and basename (string= basename "index"))
;;         (concat dirname "/" name)
;;       name)))

;; (setq-default mode-line-buffer-identification
;;               '(:eval (custom-buffer-name)))

;; ;; 移除默认的vc-mode显示
;; (setq-default mode-line-format
;;               (delq 'vc-mode mode-line-format))

;; ;; 添加自定义Git分支显示（正则匹配替换）
;; ;; 移除默认的vc-mode显示
;; (setq-default mode-line-format
;;               (assq-delete-all 'vc-mode mode-line-format))
;; (add-to-list 'mode-line-format
;;  '(:eval
;;    (when vc-mode
;;      (let ((str (format-mode-line '(vc-mode vc-mode))))
;;        (when (string-match " Git[:-]\\(.*\\)" str)
;;          (format " %s" (match-string 1 str)))))))

;; ;; 自定义mode-line中的行号和列号格式
;; (setq-default mode-line-position
;;               '((line-number-mode " %l:%c ")
;;                 (size-indication-mode " %I")))


;; ;; 在 mode-line 显示加粗彩色项目名
;; (setq-default mode-line-format
;;               (cons '(:eval (when-let ((project (project-current))
;;                                        (project-root (project-root project)))
;;                               (format " %s"
;;                                       (propertize
;;                                        (file-name-nondirectory
;;                                         (directory-file-name project-root))
;;                                        'face '(:weight bold :foreground "#98C379")))))
;;                     mode-line-format))


;;;;;;;; customize mode-line end ;;;;;;;;;;;;;;

(setq-default mode-line-format
              '("%e"
                mode-line-front-space

                ;; 1. 项目名
                (:eval (when (project-current)
                         (propertize (concat "" (project-name (project-current)) "")
                                     'face '(:weight bold :background "green"))))

                ;; 2. 文件名 + 父目录 + 修改状态
                (:eval (when buffer-file-name
                         (let* ((parent (file-name-nondirectory
                                         (directory-file-name (file-name-directory buffer-file-name))))
                                (file (file-name-nondirectory buffer-file-name))
                                (parent-file (format "%s/%s" parent file)))
                           (format " %s "
                                   (if (buffer-modified-p)
                                       (propertize parent-file 'face 'error)
                                     parent-file)))))

                ;; 5. 文件编码
                " %z "

                ;; 7. 光标位置
                " %l:%c "

                ;; 6. Major mode
                " [" mode-name "] "

                "      "
                ;; 4. Flycheck 诊断（错误/警告）
                (:eval
                 (when (bound-and-true-p flycheck-mode)
                   (let* ((counts (flycheck-count-errors flycheck-current-errors))
                          (errors (or (cdr (assq 'error counts)) 0))
                          (warnings (or (cdr (assq 'warning counts)) 0)))
                     (concat
                      (propertize (format "%dE " errors) 'face 'error)
                      (propertize (format "%dW    " warnings) 'face 'warning)))))

                ;; 3. Git 分支
                (:eval (when vc-mode
                         (let ((branch (replace-regexp-in-string "^ Git[:-]" "" vc-mode)))
                           (propertize (format "   %s " branch)
                                       'face 'font-lock-constant-face))))

                ;; 8. 当前时间
                (:eval (format-time-string "  🕒 %m-%d %H:%M"))

                mode-line-end-spaces))

(add-hook 'prog-mode-hook (lambda ()
                            ;; (set-face-attribute 'fringe nil :background "#000000") ;; setting for modus-vivendi theme
                            (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
                            (set-face-attribute 'font-lock-keyword-face nil :weight 'semi-bold :slant 'italic)
                            (set-face-attribute 'font-lock-function-name-face nil :weight 'semi-bold :slant 'italic)))


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


(use-package minions
  :vc (:fetcher github :repo tarsius/minions)
  :init
  (minions-mode 1)
  :config
  (setq minions-mode-line-lighter "👋")
  (add-to-list 'minions-prominent-modes 'flycheck-mode))

(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  ;; :hook after-init
  :hook (after-init . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '( :internal-border-width 15
      :header-line-width 4
      :mode-line-width 2
      :tab-width 4
      :right-divider-width 2
      :scroll-bar-width 8
      :fringe-width 8)))

;; beautiful compilation buffer
(use-package fancy-compilation
  :commands (fancy-compilation-mode))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

(provide 'init-theme)
