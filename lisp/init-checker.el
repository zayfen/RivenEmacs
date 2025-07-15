;; -*- coding: utf-8; lexical-binding: t -*-

;;; config checker

(use-package flymake
  :ensure nil ; 内置包
  :commands flymake-mode
  :hook (prog-mode . flymake-mode)
  :config
  ;; 键绑定重新映射
  (define-key flymake-mode-map [remap next-error] #'flymake-goto-next-error)
  (define-key flymake-mode-map [remap previous-error] #'flymake-goto-prev-error)

  ;; unbind "M-g M-n" and "M-g M-p"
  (keymap-global-unset "M-g M-n")
  (keymap-global-unset "M-g M-p")

  ;; 定义自定义的 mode-line 显示函数
  (defun my-flymake-mode-line ()
    "根据 Flymake 的错误和警告状态显示带有颜色的 mode-line 文本。"
    (let* ((error-count (flymake--lookup-type-property 'flymake-error :counter))
           (warning-count (flymake--lookup-type-property 'flymake-warning :counter))
           (errors (or error-count 0))
           (warnings (or warning-count 0))
           (text (format " %d | %d " errors warnings)))
      (cond
       ((> errors 0)
        (propertize text 'face '(:foreground "red")))
       ((> warnings 0)
        (propertize text 'face '(:foreground "yellow")))
       (t
        (propertize text 'face '(:foreground "green"))))))

  ;; 将自定义函数应用到 Flymake 的 mode-line
  (setq flymake-mode-line-format '(" " (:eval (my-flymake-mode-line))))

  ;; show diagnostic
  (when (>= emacs-major-version 31)
    (setopt flymake-show-diagnostics-at-end-of-line 'fancy))

  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-suppress-zero-counters t))


;; ESLint 支持
(use-package flymake-eslint
  :ensure t
  :hook ((js-mode . flymake-eslint-enable)
         (js-ts-mode . flymake-eslint-enable)
         (jsx-mode . flymake-eslint-enable)
         (typescript-mode . flymake-eslint-enable)
         (typescript-ts-mode . flymake-eslint-enable)
         (tsx-ts-mode . flymake-eslint-enable)
         (web-mode . flymake-eslint-enable)
         (vue-mode . flymake-eslint-enable))
  :config
  ;; 设置 ESLint 可执行文件
  (when (executable-find "oxlint")
    (setq flymake-eslint-executable-name "oxlint"))

  (defun get-eslint-path ()
    "Get the path to the local eslint executable if available, otherwise fall back to global eslint."
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (when root
                     (expand-file-name "node_modules/.bin/eslint" root))))
      (if (and eslint (file-executable-p eslint))
          eslint
        "eslint")))

  (defun my/use-eslint-from-node-modules ()
    "使用项目本地的 ESLint 可执行文件。"
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint
            (and root
                 (expand-file-name "node_modules/.bin/eslint"
                                   root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flymake-eslint-executable-name eslint))))

  (add-hook 'flymake-mode-hook #'my/use-eslint-from-node-modules))


;; 错误修复支持
(use-package flymake-quickdef
  :ensure t
  :after flymake
  :config
  ;; 定义 ESLint 修复后端
  (flymake-quickdef-backend flymake-eslint-fix
    :pre-let ((eslint-exec (or (and (boundp 'flymake-eslint-executable-name)
                                    flymake-eslint-executable-name)
                               "eslint")))
    :pre-check (lambda () (executable-find eslint-exec))
    :write-type 'file
    :proc-form (list eslint-exec "--fix" fmqd-temp-file)
    :search-regexp "^\\(.+\\)$"
    :prep-diagnostic (lambda (_match) (flymake-make-diagnostic
                                       (current-buffer)
                                       (point-min) (point-max)
                                       :note "ESLint fix applied")))

  ;; 添加 ESLint 修复命令
  (defun eslint-fix ()
    "使用 ESLint 修复当前文件。"
    (interactive)
    (let* ((eslint-exec (get-eslint-path)))
      (if (executable-find eslint-exec)
          (progn
            (shell-command (format "%s --fix %s" eslint-exec (buffer-file-name)))
            (revert-buffer t t)
            (message "ESLint fix applied"))
        (message "ESLint not found")))))


;; 增强的错误显示
;; (use-package posframe)
;; (use-package flymake-popon
;;   :ensure t
;;   :after flymake
;;   :hook (flymake-mode . flymake-popon-mode)
;;   :config
;;   (setq flymake-popon-method 'popon)

;;   ;; 设置白色背景
;;   (custom-set-faces
;;    '(flymake-popon ((t (:background "white" :foreground "black"))))
;;    '(flymake-popon-posframe-border ((t (:background "white"))))))

(provide 'init-checker)
;;; init-checker.el ends here
