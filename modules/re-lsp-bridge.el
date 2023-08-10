;;; tools/lsp/+lsp-bridge.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 zayfen

;; Author zayfen (zhangyunfeng0101@gmail.com)

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (use-package yasnippet-snippets :after yasnippet)
  :hook ((prog-mode LaTeX-mode org-mode markdown-mode) . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
  (:map yas-keymap
        (("TAB" . smarter-yas-expand-next-field)
         ([(tab)] . smarter-yas-expand-next-field)))
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-reload-all)
  (defun smarter-yas-expand-next-field ()
    "Try to `yas-expand' then `yas-next-field' at current cursor position."
    (interactive)
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick)))
      (yas-expand)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick)))
        (ignore-errors (yas-next-field))))))


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
  :straight (lsp-bridge
             :type git
             :host github
             :repo "manateelazycat/lsp-bridge"
             :files ("*" (:exclude ".git"))
             :build nil)
  :init
  (add-to-list 'load-path (straight--repos-dir "lsp-bridge"))
  :hook ((prog-mode) . lsp-bridge-mode)
  :custom
  (lsp-bridge-signature-function 'eldoc-message)
  (lsp-bridge-single-lang-server-extension-list '(("vue" . "volar")))
  (lsp-bridge-multi-lang-server-extension-list
   '(
     (("ts" "tsx") . "typescript_eslint")
     (("scss" "sass" "less") . "css_emmet")
;;     ("vue" . "volar_emmet")
     ))
  :bind (:map lsp-bridge-mode-map
         ("M-." . lsp-bridge-find-def)
         ("M-," . lsp-bridge-find-def-return)
         ("M-?" . lsp-bridge-find-references))
  :config
  (use-package lsp-bridge-jdtls)
  (setq acm-enable-icon t)
  (setq acm-enable-yas t)
  (setq acm-enable-doc t)
  (setq acm-enable-doc-markdown-render t)
  (setq acm-enable-path t)
  (setq acm-backend-yas-match-by-trigger-keyword t)
  (setq acm-enable-tabnine nil)
  (setq acm-enable-codeium nil)
  (setq lsp-bridge-enable-diagnostics nil) ;; we use flycheck only
  (setq lsp-bridge-enable-hover-diagnostic nil)
  (setq lsp-bridge-code-action-enable-popup-menu nil)
  (setq lsp-bridge-python-multi-lsp-server "pyright_ruff")
  (setq acm-enable-quick-access t)
  (setq acm-quick-access-use-number-select nil)
  (setq lsp-bridge-find-def-fallback #'dumb-jump-go)
  (setq lsp-bridge-find-ref-fallback #'xref-find-references)
  (setq lsp-bridge-symbols-enable-which-func t)
  (setq lsp-bridge-signature-show-function t)

  (+map! :keymaps 'lsp-bridge-mode-map
    :infix "c"
    "a"  '(lsp-bridge-code-action :wk "Code actions")
    "e"  '(lsp-bridge-diagnostic-list :wk "Diagnostic list")
    "f" '(lsp-bridge-code-format :wk "Format code")
    "F" '(lsp-bridge-code-action--fix :wk "Quick fix")
    "i"  '(lsp-bridge-find-impl :wk "Find implementation")
    "k"  '(lsp-bridge-popup-documentation :wk "Find Document")
    "p"  '(lsp-bridge-peek :wk "Peek")
    "r" '(lsp-bridge-rename :wk "Rename")
    "t"  '(lsp-bridge-find-type-def :wk "Find type definition"))
  )


(provide 're-lsp-bridge)
