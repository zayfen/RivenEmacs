;;; -*- coding: utf-8; lexical-binding: t -*-

(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  (defun create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'jtsx-tsx-mode "<" nil :actions nil)
  (sp-local-pair 'jtsx-jsx-mode "<" nil :actions nil)

  (require 'smartparens-config))

(provide 'init-pair)
