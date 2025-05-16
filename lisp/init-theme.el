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

(setq-default mode-line-format
              '("%e" ; 错误信息，通常为空
                mode-line-front-space

                ;; 1. 项目名 (背景更宽，颜色调整)
                (:eval (when (project-current)
                         (let ((project-name-str (project-name (project-current))))
                           (propertize (concat " " project-name-str " ") ; 前后加空格使其更宽
                                       'face `(:weight bold :background "Peru" :foreground "white")))))
                ;; 右边半圆形较难直接实现，加宽背景可部分达到视觉效果
                ;; 如果需要真实半圆，可能需要更复杂的powerline字符或图像

                ;; 2. 文件名 + 父目录 + 修改状态 (颜色优化)
                (:eval (when buffer-file-name
                         (let* ((parent (file-name-nondirectory
                                         (directory-file-name (file-name-directory buffer-file-name))))
                                (file (file-name-nondirectory buffer-file-name))
                                (parent-file (format "%s/%s" parent file))
                                (text-color (cond
                                             (buffer-read-only "gray60")      ; 只读文件用灰色
                                             ((buffer-modified-p) "coral")    ; 修改过的文件用珊瑚红
                                             (t "white")))                     ; 正常文件用白色 (假设深色modeline背景)
                                (current-face `(:foreground ,text-color)))
                           (when (buffer-modified-p)
                             (setq current-face (append current-face '(:weight bold)))) ; 修改过的加粗
                           (format " %s " (propertize parent-file 'face current-face)))))

                ;; 5. 文件编码 (圆形背景，颜色调整)
                (:eval (let ((coding-str (symbol-name buffer-file-coding-system)))
                         ;; 通过前后空格和短字符串来模拟"圆形"或"药丸形"背景
                         (propertize (format " %s " coding-str)
                                     'face '(:background "dodgerblue4" :foreground "white" :weight semi-bold))))

                ;; 7. 光标位置 (颜色优化)
                (:eval (propertize " %l:%c " 'face '(:foreground "gray70")))

                ;; 6. Major mode (颜色优化)
                (:eval (let ((formatted-mode-name (format-mode-line mode-name)))
                         (propertize (concat " " formatted-mode-name " ")
                                     'face '(:foreground "PaleGreen" :weight semi-bold))))

                (:eval (let ((formatted-mode-name (format-mode-line local-minor-modes)))
                         (propertize (concat "" formatted-mode-name " ")
                                     'face '(:foreground "PaleGreen" :weight semi-bold))))

                ;; show minor mode

                "  " ; 静态间隔

                ;; 4. Flycheck 诊断（错误/警告） (使用标准face，颜色通常由主题定义)
                (:eval
                 (when (bound-and-true-p flycheck-mode)
                   (let* ((counts (flycheck-count-errors flycheck-current-errors))
                          (errors (or (cdr (assq 'error counts)) 0))
                          (warnings (or (cdr (assq 'warning counts)) 0)))
                     (concat
                      (propertize (format " %d " errors) 'face 'flycheck-error) ; 使用 flycheck 定义的 face
                      (propertize (format " %d " warnings) 'face 'flycheck-warning))))) ; 使用 flycheck 定义的 face

                ;; 用于右对齐的动态空格
                ;; 这个空格会占据 Flycheck 和 Git 分支之间的所有可用空间
                ;; 使 Git 分支和时间部分靠右显示
                '(:eval
                  (let* ((git-string
                          (if (and vc-mode vc-mode) ; 确保 vc-mode 不为 nil
                              (let ((branch (replace-regexp-in-string "^ Git[:-]" "" vc-mode)))
                                (format "    %s " branch))
                            ""))
                         (time-string (format-time-string "  🕒 %m-%d %H:%M"))
                         (right-elements-width (+ (string-width git-string)
                                                  (string-width time-string)
                                                  2))) ; 额外一点padding
                    (propertize " " 'display `((space :align-to (- right-edge ,right-elements-width))))))


                ;; 3. Git 分支 (颜色优化, Nerd Font 图标需要安装对应字体)
                (:eval (when vc-mode
                         (let ((branch (replace-regexp-in-string "^ Git[:-]" "" vc-mode)))
                           (propertize (format " %s " branch)
                                       'face '(:foreground "OrangeRed" :weight bold))))) ; 显眼的颜色

                ;; 8. 当前时间 (颜色优化)
                (:eval (propertize (format-time-string "  🕒 %m-%d %H:%M") 'face '(:foreground "LightSteelBlue")))
                "    "
                mode-line-process
                " "
                ;; minor-mode-alist ;;enable this to show minor modes
                ;; Others
                mode-line-end-spaces))



(add-hook 'after-init-hook
          (lambda ()
            (progn
              ;; Customize active mode-line
              (set-face-attribute 'mode-line nil
                                  :background "#1a1a1a")

              ;; Customize inactive mode-line
              (set-face-attribute 'mode-line-inactive nil
                                  :background "#2a2a2a"))))
;;;;;;;; customize mode-line end ;;;;;;;;;;;;;;


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


;; (use-package minions
;;   :vc (:fetcher github :repo tarsius/minions)
;;   :init
;;   (minions-mode 1)
;;   :config
;;   (setq minions-mode-line-lighter "👋")
;;   (add-to-list 'minions-prominent-modes 'flycheck-mode))

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
      :fringe-width 2)))


;; beautiful compilation buffer
(use-package fancy-compilation
  :commands (fancy-compilation-mode))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

;; change line number color
;; (set-face-foreground 'line-number "#808080")

(provide 'init-theme)
;;; init-theme.el ends here
