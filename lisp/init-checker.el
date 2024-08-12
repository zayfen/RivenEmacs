;; -*- coding: utf-8; lexical-binding: t -*-

;;; config checker

;; (use-package flymake
;;   :hook
;;   (flymake-mode-hook . flymake-setup-next-error-function)
;;   :commands
;;   (flymake-show-diagnostic
;;    flymake-goto-next-error)
;;   :bind
;;   ((:map flymake-mode-map
;;          ("M-g n" . flymake-goto-next-error)
;;          ("M-g p" . flymake-goto-prev-error))
;;    (:repeat-map flymake-mode-repeat-map
;;                ("e" . flymake-goto-next-error)
;;                ("E" . flymake-goto-prev-error)))

;;   :config
;;   (defun flymake-setup-next-error-function ()
;;     (setq next-error-function 'flymake-next-error-compat))

;;   (defun flymake-next-error-compat (&optional n _)
;;     (flymake-goto-next-error n))

;;   (defun flymake-diagnostics-next-error ()
;;     (interactive)
;;     (forward-line)
;;     (when (eobp) (forward-line -1))
;;     (flymake-show-diagnostic (point)))

;;   (defun flymake-diagnostics-prev-error ()
;;     (interactive)
;;     (forward-line -1)
;;     (flymake-show-diagnostic (point))))

;; (use-package flymake-proc
;;   :after flymake
;;   :config
;;   (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))


(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-initialize-packages t
                flycheck-highlighting-mode 'lines)

  (flycheck-add-mode 'javascript-eslint 'jtsx-tsx-mode)
  (flycheck-add-mode 'javascript-eslint 'jtsx-typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'jtsx-jsx-mode)
  (define-key flycheck-mode-map [remap next-error] #'flycheck-next-error)
  (define-key flycheck-mode-map [remap previous-error] #'flycheck-previous-error)

  :custom
  (flycheck-indication-mode 'left-fringe)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled)))


(when (executable-find "oxlint")
  (setq flycheck-javascript-eslint-executable "oxlint"))



(provide 'init-checker)
