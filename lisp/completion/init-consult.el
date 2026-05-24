;; -*- coding: utf-8; lexical-binding: t -*-

;;; init-consult.el --- Consult and Embark completion commands

;;; Code:

(require 'init-minibuffer)
(require 'init-completion-ui)

(use-package consult
  :vc (:url "https://github.com/minad/consult")
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-s" . consult-line)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x r b" . consult-bookmark)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g i" . consult-imenu)
         ;; M-s bindings in `search-map'
         ("M-s g" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (when (fboundp 'consult--customize-put)
    (consult--customize-put
     '(consult-buffer consult-project-buffer)
     :preview-key
     '"M-.")
    (consult--customize-put
     '(consult-source-bookmark
       consult-source-file-register
       consult-source-recent-file
       consult-source-project-recent-file
       consult-source-project-recent-file-hidden)
     :preview-key
     '"M-."))
  (dolist (source '(consult-source-file-register
                    consult-source-recent-file
                    consult-source-project-recent-file-hidden))
    (when (boundp source)
      (setf (plist-get (symbol-value source) :preview-key) "M-.")
      (setf (plist-get (symbol-value source) :state) #'consult--file-state)))
  (when (boundp 'consult-source-bookmark)
    (setf (plist-get (symbol-value 'consult-source-bookmark) :preview-key) "M-.")
    (setf (plist-get (symbol-value 'consult-source-bookmark) :state) #'consult--bookmark-state))
  :custom
  (consult-buffer-filter '("\\` "
                           "\\`\\*.*\\*\\'")))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-c ;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-consult)
;;; init-consult.el ends here
