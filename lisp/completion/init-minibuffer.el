;; -*- coding: utf-8; lexical-binding: t -*-

;;; init-minibuffer.el --- Minibuffer completion stack

;;; Code:

;; Previously this module manually scanned the ELPA directory tree with
;; `directory-files' to populate `load-path' for completion packages.
;; `package-initialize' is invoked in `early-init.el', so every installed
;; package's directory is already on `load-path'; the manual scan was redundant
;; startup work and has been removed.

(use-package vertico
  :vc (:url "https://github.com/minad/vertico")
  :demand t
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  :config
  (vertico-mode))

(use-package vertico-multiform
  :if (locate-library "vertico-multiform")
  :after vertico
  :ensure nil
  :config
  (require 'vertico-buffer nil t)
  (require 'vertico-indexed nil t)
  (require 'vertico-unobtrusive nil t)
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((consult-imenu buffer indexed)))
  (setq vertico-multiform-categories
        '((consult-grep buffer))))

(with-eval-after-load 'vertico
  (if (require 'vertico-directory nil t)
      (progn
        (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
        (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
        (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
        (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))
    (define-key vertico-map (kbd "DEL") #'backward-delete-char-untabify)))

(use-package savehist
  :demand t
  :init
  (savehist-mode))

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  (completion-cycle-threshold 3)
  (completion-cycling t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (setq completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t)
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :vc (:url "https://github.com/minad/marginalia")
  :demand t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

(use-package prescient
  :demand t
  :config (prescient-persist-mode))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
