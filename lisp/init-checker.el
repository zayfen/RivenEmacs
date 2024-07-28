;; -*- coding: utf-8; lexical-binding: t -*-

;;; config checker

(use-package flymake
  :hook
  (flymake-mode-hook . flymake-setup-next-error-function)
  :commands
  (flymake-show-diagnostic
   flymake-goto-next-error)
  :bind
  ((:map flymake-mode-map
         ("M-g n" . flymake-goto-next-error)
         ("M-g p" . flymake-goto-prev-error))
   (:repeat-map flymake-mode-repeat-map
               ("e" . flymake-goto-next-error)
               ("E" . flymake-goto-prev-error)))
  :config
  (defun flymake-setup-next-error-function ()
    (setq next-error-function 'flymake-next-error-compat))

  (defun flymake-next-error-compat (&optional n _)
    (flymake-goto-next-error n))

  (defun flymake-diagnostics-next-error ()
    (interactive)
    (forward-line)
    (when (eobp) (forward-line -1))
    (flymake-show-diagnostic (point)))

  (defun flymake-diagnostics-prev-error ()
    (interactive)
    (forward-line -1)
    (flymake-show-diagnostic (point))))

(use-package flymake-proc
  :after flymake
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))


(provide 'init-checker)
