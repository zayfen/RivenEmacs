;; -*- coding: utf-8; lexical-binding: t -*-

;;; config checker



(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-initialize-packages t
                flycheck-highlighting-mode 'lines)

  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'jsx-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'vue-mode)
  (define-key flycheck-mode-map [remap next-error] #'flycheck-next-error)
  (define-key flycheck-mode-map [remap previous-error] #'flycheck-previous-error)

  ;; unbind "M-g M-n" and "M-g M-p"
  (keymap-global-unset "M-g M-n")
  (keymap-global-unset "M-g M-p")

  ;; 定义自定义的 mode-line 显示函数
  (defun my-flycheck-mode-line ()
    "根据 Flycheck 的错误和警告状态显示带有颜色的 mode-line 文本。"
    (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
           (errors (or (cdr (assq 'error error-counts)) 0))
           (warnings (or (cdr (assq 'warning error-counts)) 0))
           (text (format " %d | %d " errors warnings)))
      (cond
       ((> errors 0)
        (propertize text 'face '(:foreground "red")))
       ((> warnings 0)
        (propertize text 'face '(:foreground "yellow")))
       (t
        (propertize text 'face '(:foreground "green"))))))

  ;; 将自定义函数应用到 Flycheck 的 mode-line
  (setq flycheck-mode-line '(:eval (my-flycheck-mode-line)))

  :custom
  (flycheck-indication-mode 'left-fringe)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled)))


(use-package flyover
  :vc (:url "https://github.com/konrad1977/flyover.git")
  :after flycheck
  :hook (flycheck-mode . flyover-mode)
  :config
  (setq flyover-levels '(error warning))
  (setq flyover-use-theme-colors t)
  (setq flyover-text-tint 'lighter)
  (setq flyover-checkers '(flycheck flymake)))


(when (executable-find "oxlint")
  (setq flycheck-javascript-eslint-executable "oxlint"))


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
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint
          (and root
               (expand-file-name "node_modules/.bin/eslint"
                                 root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; install eslint-fix package

(use-package eslint-fix
  :vc (:url "https://github.com/codesuki/eslint-fix")
  :commands (eslint-fix))


(provide 'init-checker)
