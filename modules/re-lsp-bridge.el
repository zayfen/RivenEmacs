;;; tools/lsp/+lsp-bridge.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 zayfen

;; Author zayfen (zhangyunfeng0101@gmail.com)

(use-package yasnippet
  :ensure t
  :straight t
  :bind (("M-i" . yas-insert-snippet)
         ("M-]" . yas-expand))
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode 1))


(use-package yasnippet-snippets
  :straight t
  :ensure t
  :after (yasnippet))

(use-package markdown-mode
  :ensure t
  :straight t)

(use-package posframe
  :ensure t
  :straight t)


(use-package lsp-bridge
  :ensure t
  :straight (:host github
                   :repo "zayfen/lsp-bridge"
                   :files ("*" (:exclude ".git")))
  :init

  :hook ((prog-mode) . lsp-bridge-mode)

  :custom
  (lsp-bridge-signature-function 'eldoc-message)

  (lsp-bridge-multi-lang-server-extension-list
   '((("ts" "tsx") . "typescript_eslint")))

  :config
  (use-package lsp-bridge-jdtls)
  (setq acm-enable-icon t)
  (setq acm-enable-yas t)
  (setq acm-enable-doc t)
  (setq acm-enable-doc-markdown-render t)
  (setq acm-enable-path t)
  (setq acm-backend-lsp-candidate-min-length 2)
  (setq acm-doc-frame-max-lines 30)
  (setq acm-backend-yas-match-by-trigger-keyword t)
  (setq lsp-bridge-enable-diagnostics t)
  (setq lsp-bridge-enable-hover-diagnostic t)
  (setq lsp-bridge-code-action-enable-popup-menu nil)
  (setq lsp-bridge-python-multi-lsp-server "pyright_ruff")


  (+map! :keymaps 'lsp-bridge-mode-map
    :infix "c"
    "a"  '(lsp-bridge-code-action :wk "Code actions")
    "d"  '(lsp-bridge-find-def :wk "Find definition")
    "e"  '(lsp-bridge-diagnostic-list :wk "Diagnostic list")
    "t"  '(lsp-bridge-find-type-def :wk "Find type definition")
    "fF" #'lsp-bridge-code-format
    "i"  '(lsp-bridge-find-impl :wk "Find implementation")
    "k"  '(lsp-bridge-popup-documentation :wk "Find Document")
    "x" '(lsp-bridge-workspace-list-symbols :wk "Symbols & Jump to define")
    "?"  '(lsp-bridge-find-references :wk "Find References")


    "r"  '(nil :wk "refactor")
    "rf" '(lsp-bridge-code-action--fix :wk "Quick fix")
    "rr" '(lsp-bridge-rename :wk "Rename")
    "ld" '(lsp-bridge-toggle-sdcv-helper :wk "Toggle Dictionary")
    "lr" '(lsp-workspace-restart :wk "Restart"))



   ;; 融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
  (defun lsp-bridge-jump ()
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (let ((symb (function-called-at-point)))
        (when symb
          (find-function symb))))
     (lsp-bridge-mode
      (lsp-bridge-find-def))
     (t
      (require 'dumb-jump)
      (dumb-jump-go))))

  (defun lsp-bridge-jump-back ()
    (interactive)
    (cond
     (lsp-bridge-mode
      (lsp-bridge-find-def-return))
     (t
      (require 'dumb-jump)
      (dumb-jump-back)))))

(provide 're-lsp-bridge)
