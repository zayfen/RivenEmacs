;; -*- coding: utf-8; lexical-binding: t -*-

;;; config checker

(use-package flymake
  :ensure nil ; 内置包
  :commands flymake-mode
  :hook (prog-mode . flymake-mode)
  :config
  (defvar-local riven/flymake-last-echo-line nil
    "Last line number used for Flymake minibuffer echo.")

  (defvar-local riven/flymake-last-echo-text nil
    "Last Flymake diagnostic text shown in minibuffer.")

  (defvar-local riven/flymake--echo-timer nil
    "Idle timer used to debounce Flymake minibuffer echo in this buffer.")

  (defcustom riven/flymake-echo-idle-delay 0.1
    "Idle seconds before echoing the current line's Flymake diagnostic.
Avoids running `flymake-diagnostics' on every keystroke."
    :type 'number
    :group 'rivenEmacs)

  (defun riven/flymake-current-line-message ()
    "Return summarized Flymake diagnostics for current line, or nil."
    (let* ((diags (flymake-diagnostics (line-beginning-position) (line-end-position)))
           (texts (delete-dups
                   (delq nil (mapcar #'flymake-diagnostic-text diags)))))
      (when texts
        (if (= (length texts) 1)
            (car texts)
          (format "%s (+%d more)" (car texts) (1- (length texts)))))))

  (defun riven/flymake-echo-current-line ()
    "Display current line Flymake diagnostic in minibuffer.
Cheap short-circuit on the line number avoids the expensive
`flymake-diagnostics' call when point hasn't left the last echoed line."
    (when (and flymake-mode
               (not (active-minibuffer-window))
               (not (derived-mode-p 'flymake-diagnostics-buffer-mode
                                    'flymake-project-diagnostics-mode)))
      (let ((line (line-number-at-pos)))
        ;; Cheap check first: same line → nothing changed, skip the heavy call.
        (unless (equal line riven/flymake-last-echo-line)
          (let* ((text (riven/flymake-current-line-message))
                 (had-text riven/flymake-last-echo-text))
            (setq riven/flymake-last-echo-line line
                  riven/flymake-last-echo-text text)
            (unless (equal text had-text)
              (let ((message-log-max nil))
                (cond
                 (text (message "%s" text))
                 (had-text (message nil))))))))))

  (defun riven/flymake--echo-schedule ()
    "Schedule a debounced Flymake echo on the current buffer."
    (when (and flymake-mode (buffer-live-p (current-buffer)))
      (when riven/flymake--echo-timer
        (cancel-timer riven/flymake--echo-timer))
      (setq riven/flymake--echo-timer
            (run-with-idle-timer riven/flymake-echo-idle-delay nil
                                 #'riven/flymake-echo-current-line))))

  (defun riven/flymake-toggle-echo-hook ()
    "Toggle minibuffer diagnostic echo hook with `flymake-mode'.
Uses a debounced idle timer instead of running on every `post-command-hook',
so `flymake-diagnostics' is only consulted when the user pauses."
    (if flymake-mode
        (add-hook 'post-command-hook #'riven/flymake--echo-schedule nil t)
      (when riven/flymake--echo-timer
        (cancel-timer riven/flymake--echo-timer)
        (setq riven/flymake--echo-timer nil))
      (remove-hook 'post-command-hook #'riven/flymake--echo-schedule t)
      (kill-local-variable 'riven/flymake-last-echo-line)
      (kill-local-variable 'riven/flymake-last-echo-text)))

  (add-hook 'flymake-mode-hook #'riven/flymake-toggle-echo-hook)
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when riven/flymake--echo-timer
                (cancel-timer riven/flymake--echo-timer))))

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
  ;; NOTE: `flymake-show-diagnostics-at-end-of-line' is intentionally `t'
  ;; (see `:custom' below and commit e856045). It used to also be set to `nil'
  ;; in this `:config' block, which was dead code — `:custom' runs after
  ;; `:config' so `t' always won, but the contradiction was misleading. The
  ;; contradictory setopt has been removed so the effective value is unambiguous.
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-show-diagnostics-at-end-of-line t)
  (flymake-suppress-zero-counters t))


;; ESLint 支持
(use-package flymake-eslint
  :ensure t
  :hook ((js-mode . +flymake-eslint-enable-safe)
         (js-ts-mode . +flymake-eslint-enable-safe)
         (jsx-mode . +flymake-eslint-enable-safe)
         (typescript-mode . +flymake-eslint-enable-safe)
         (typescript-ts-mode . +flymake-eslint-enable-safe)
         (tsx-ts-mode . +flymake-eslint-enable-safe)
         (web-mode . +flymake-eslint-enable-safe)
         (vue-ts-mode . +flymake-eslint-enable-safe))
  :config
  (defun +flymake-eslint-enable-safe ()
    "Enable flymake-eslint only when an eslint executable is available."
    (when (or (executable-find (or flymake-eslint-executable-name "eslint"))
              (let* ((root (locate-dominating-file
                            (or (buffer-file-name) default-directory)
                            "node_modules"))
                     (local (when root
                              (expand-file-name "node_modules/.bin/eslint" root))))
                (and local (file-executable-p local))))
      (flymake-eslint-enable)))

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
