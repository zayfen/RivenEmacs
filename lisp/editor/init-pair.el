;;; -*- coding: utf-8; lexical-binding: t -*-

(defun rivenEmacs-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the current buffer.
Used as the hook function for programming/text modes so a missing
package or autoload failure cannot break the mode hooks."
  (smartparens-mode 1))

(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook ((prog-mode text-mode markdown-mode) . rivenEmacs-enable-smartparens-mode)
  :config
  (defun create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'web-mode "{" nil :post-handlers '((create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'tsx-ts-mode "<" nil :actions nil)
  (sp-local-pair 'js-ts-mode "<" nil :actions nil)
  (sp-local-pair 'javascript-mode "<" nil :actions nil)
  (sp-local-pair 'typescript-ts-mode "<" nil :actions nil)
  (sp-local-pair 'typescript-mode "<" nil :actions nil)

  (require 'smartparens-config))

(provide 'init-pair)
