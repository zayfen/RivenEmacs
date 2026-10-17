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
;; `+flymake-eslint-enable-safe' must exist before the mode hooks run, so
;; define it at top level (a `defun' inside `:config' is never defined when
;; `use-package-always-defer' is active).  The package is required lazily on
;; the first JS-like buffer.
(defun +flymake-eslint-enable-safe ()
  "Enable flymake-eslint only when an eslint executable is available."
  (when (require 'flymake-eslint nil t)
    (when (or (executable-find
               (or (and (boundp 'flymake-eslint-executable-name)
                        flymake-eslint-executable-name)
                   "eslint"))
              (let* ((root (locate-dominating-file
                            (or (buffer-file-name) default-directory)
                            "node_modules"))
                     (local (when root
                              (expand-file-name "node_modules/.bin/eslint" root))))
                (and local (file-executable-p local))))
      (flymake-eslint-enable))))

(use-package flymake-eslint
  :ensure t
  :hook ((js-mode . +flymake-eslint-enable-safe)
         (js-ts-mode . +flymake-eslint-enable-safe)
         (jsx-mode . +flymake-eslint-enable-safe)
         (typescript-mode . +flymake-eslint-enable-safe)
         (typescript-ts-mode . +flymake-eslint-enable-safe)
         (tsx-ts-mode . +flymake-eslint-enable-safe)
         (web-mode . +flymake-eslint-enable-safe)
         (vue-ts-mode . +flymake-eslint-enable-safe)))

;; These three definitions live at top level (not in `flymake-eslint's
;; `:config'): `use-package-always-defer' means that config only runs when
;; the package is actually loaded, and `flymake-quickdef's config never runs
;; at all because nothing loads it.  Defining them here keeps `eslint-fix'
;; and friends available on startup.
(defun get-eslint-path ()
  "Return the path to the local ESLint executable, or \"eslint\".
Uses the project-local `node_modules/.bin/eslint' when it exists and is
executable, otherwise falls back to the global eslint command."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (when root
                   (expand-file-name "node_modules/.bin/eslint" root))))
    (if (and eslint (file-executable-p eslint))
        eslint
      "eslint")))

(defun my/use-eslint-from-node-modules ()
  "Use the project-local ESLint executable when available."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint
          (and root
               (expand-file-name "node_modules/.bin/eslint"
                                 root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flymake-eslint-executable-name eslint))))

(add-hook 'flymake-mode-hook #'my/use-eslint-from-node-modules)


;; 错误修复支持
(use-package flymake-quickdef
  :ensure t
  ;; `:demand t' is required: without it `use-package-always-defer' wraps
  ;; this `:config' in an `eval-after-load' that never fires, so the
  ;; `flymake-eslint-fix' backend would never be registered.
  :demand t
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
                                       :note "ESLint fix applied"))))

(defun eslint-fix ()
  "Fix the current file with ESLint.
Runs the project-local or global ESLint executable with `--fix' and
shows its output when it exits non-zero.  The buffer must be visiting a
saved file; save it first so ESLint edits the same content that Emacs
will reload."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (when (buffer-modified-p)
      (user-error "Save the buffer before running ESLint fix"))
    (let ((eslint (get-eslint-path)))
      (unless (executable-find eslint)
        (user-error "ESLint not found; install it or add it to PATH"))
      (let ((output (get-buffer-create "*eslint-fix*")))
        (with-current-buffer output
          (erase-buffer))
        (let ((status (condition-case err
                          (process-file eslint nil output nil "--fix" file)
                        (error
                         (user-error "Failed to run ESLint: %s"
                                     (error-message-string err))))))
          (if (and (integerp status) (zerop status))
              (progn
                (revert-buffer t t t)
                (message "ESLint fix applied"))
            (display-buffer output)
            (user-error "ESLint failed (exit %s); see %s"
                        status (buffer-name output))))))))


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
