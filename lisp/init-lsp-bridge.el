;;; -*- coding: utf-8; lexical-binding: t -*-

(use-package yasnippet
  :ensure t
  :custom
  (yas-snippet-dir "~/.emacs.d/snippets")
  :hook ((prog-mode LaTeX-mode org-mode markdown-mode) . yas-minor-mode-on)
  :bind
  (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
  (:map yas-keymap
        (("TAB" . smarter-yas-expand-next-field)
         ([(tab)] . smarter-yas-expand-next-field)))

  :config
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
  :ensure t)

(require 'mark-power)
(defun find-definitions-with-lsp-bridge ()
  (interactive)
  (mark-power--set-mark "find-def")
  (lsp-bridge-find-def))

(defun find-impl-with-lsp-bridge ()
  (interactive)
  (mark-power--set-mark "find-impl")
  (lsp-bridge-find-impl))

(defun find-peek-with-lsp-bridge ()
  (interactive)
  (mark-power--set-mark "find-peek")
  (lsp-bridge-peek-jump))

(defun find-typedef-with-lsp-bridge ()
  (interactive)
  (mark-power--set-mark "find-typedef")
  (lsp-bridge-find-type-def))

(use-package lsp-bridge
  :vc (:fetcher github :repo "manateelazycat/lsp-bridge")
  :hook ((prog-mode) . lsp-bridge-mode)
  :custom
  (lsp-bridge-python-lsp-server "ruff")
  (lsp-bridge-python-multi-lsp-server "pyright_ruff")
  (lsp-bridge-completion-obey-trigger-characters-p nil)
  (lsp-bridge-completion-hide-characters '(":" ";" "(" ")" "[" "]" "{" "}" ", " "\"" "?" ","))
  (lsp-bridge-multi-lang-server-extension-list
   '(
     (("ts") . "typescript_eslint")
     (("tsx") . "typescriptreact_eslint")
     (("css" "scss" "sass" "less") . "css_emmet")
     ))
  :bind (:map lsp-bridge-mode-map
              ("M-." . find-definitions-with-lsp-bridge)
              ("M-," . mark-power--jump-back)
              ("M-?" . lsp-bridge-find-references)
              ("M-<up>" . lsp-bridge-popup-documentation-scroll-down)
              ("M-<down>" . lsp-bridge-popup-documentation-scroll-up))
  :bind (:map acm-mode-map
              ("M-p" . acm-doc-scroll-down)
              ("M-n" . acm-doc-scroll-up))
  :bind (:map lsp-bridge-peek-keymap
              ("M-p" . lsp-bridge-peek-list-prev-line)
              ("M-n" . lsp-bridge-peek-list-next-line)
              ("M-<return>" . find-peek-with-lsp-bridge))
  :config
  (setq acm-enable-icon t)
  (setq acm-enable-yas t)
  (setq acm-enable-doc t)
  (setq acm-enable-doc-markdown-render t)
  (setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
  (setq acm-enable-path t)
  (setq acm-backend-yas-match-by-trigger-keyword t)
  (setq acm-enable-tabnine nil)
  (setq acm-enable-codeium nil)
  (setq acm-enable-preview t)
  (setq acm-enable-capf t)
  (setq acm-backend-search-file-words-enable-fuzzy-match t)
  (setq lsp-bridge-enable-diagnostics nil) ;; we use flycheck only
  (setq lsp-bridge-enable-hover-diagnostic t)
  (setq lsp-bridge-code-action-enable-popup-menu nil)
  (setq acm-enable-quick-access t)
  (setq lsp-bridge-find-ref-fallback #'xref-find-references)
  (setq lsp-bridge-inlay-hint t)

  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("html") . "html_tailwindcss"))
  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("css") . "css_tailwindcss"))

  (leader-def :keymaps 'lsp-bridge-mode-map
    :infix "c"
    "" '(:ignore t :wk "Code")
    "a"  '(lsp-bridge-code-action :wk "Code actions")
    "e"  '(lsp-bridge-diagnostic-list :wk "Diagnostic list")
    "f" '(lsp-bridge-code-format :wk "Format code")
    "i"  '(find-impl-with-lsp-bridge :wk "Find implementation")
    "k"  '(lsp-bridge-popup-documentation :wk "Find Document")
    "p"  '(lsp-bridge-peek :wk "Peek")
    "r" '(lsp-bridge-rename :wk "Rename")
    "t"  '(find-typedef-with-lsp-bridge :wk "Find type definition")
    "?" '(lsp-bridge-find-references :wk "Find References")))


(provide 'init-lsp-bridge)
