;;; tools/lsp/+lsp-bridge.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 zayfen

;; Author zayfen (zhangyunfeng0101@gmail.com)

;;; Code

(use-package yasnippet
  :straight t
  :ensure t
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets")
  :hook ((prog-mode LaTeX-mode org-mode markdown-mode) . yas-minor-mode-on)
  :bind
  (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
  (:map yas-keymap
        (("TAB" . smarter-yas-expand-next-field)
         ([(tab)] . smarter-yas-expand-next-field)))

  :config
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
  :ensure t)

(use-package markdown-mode
  :ensure t
  :straight t)

(use-package posframe
  :ensure t
  :straight t)


(defun find-definitions-with-lsp-bridge ()
  (interactive)
  (if lsp-bridge-mode (lsp-bridge-find-def)
    (call-interactively 'xref-find-definitions)))

(advice-add #'lsp-bridge-find-def :around (lambda (&rest args) (xref--push-markers) (apply args)))
(advice-add #'lsp-bridge-find-impl :around (lambda (&rest args) (xref--push-markers) (apply args)))
(advice-add #'lsp-bridge-find-type-def :around (lambda (&rest args) (xref--push-markers) (apply args)))



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
  (lsp-bridge-python-lsp-server "ruff")
  (lsp-bridge-python-multi-lsp-server "pyright_ruff")
  (lsp-bridge-completion-obey-trigger-characters-p nil)
  (lsp-bridge-completion-hide-characters '(":" ";" "(" ")" "[" "]" "{" "}" ", " "\"" "?" ","))
  (lsp-bridge-multi-lang-server-extension-list
   '(
     (("ts" "tsx") . "typescript_eslint")
     (("css" "scss" "sass" "less") . "css_emmet")
     ))
  :bind (:map lsp-bridge-mode-map
              ("M-." . find-definitions-with-lsp-bridge)
              ;; ("M-," . lsp-bridge-find-def-return)
              ("M-?" . lsp-bridge-find-references)
              ("M-<up>" . lsp-bridge-popup-documentation-scroll-down)
              ("M-<down>" . lsp-bridge-popup-documentation-scroll-up))
  :bind (:map acm-mode-map
              ("M-p" . acm-doc-scroll-down)
              ("M-n" . acm-doc-scroll-up))
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
  (setq acm-enable-preview t)
  (setq acm-backend-search-file-words-enable-fuzzy-match t)
  (setq lsp-bridge-enable-diagnostics nil) ;; we use flycheck only
  (setq lsp-bridge-enable-hover-diagnostic nil)
  (setq lsp-bridge-code-action-enable-popup-menu nil)
  (setq acm-enable-quick-access t)
  (setq lsp-bridge-find-def-fallback #'dumb-jump-go)
  (setq lsp-bridge-find-ref-fallback #'xref-find-references)


  (+map! :keymaps 'lsp-bridge-mode-map
    :infix "c"
    "a"  '(lsp-bridge-code-action :wk "Code actions")
    "e"  '(lsp-bridge-diagnostic-list :wk "Diagnostic list")
    "f" '(lsp-bridge-code-format :wk "Format code")
    "i"  '(lsp-bridge-find-impl :wk "Find implementation")
    "k"  '(lsp-bridge-popup-documentation :wk "Find Document")
    "p"  '(lsp-bridge-peek :wk "Peek")
    "q" '(lsp-bridge-code-action--fix :wk "Quick fix")
    "r" '(lsp-bridge-rename :wk "Rename")
    "t"  '(lsp-bridge-find-type-def :wk "Find type definition")
    "?" '(lsp-bridge-find-references :wk "Find References")))


(provide 're-lsp-bridge)
