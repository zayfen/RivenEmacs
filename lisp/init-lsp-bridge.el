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

(use-package lsp-bridge
  :vc (:fetcher github :repo "manateelazycat/lsp-bridge")
  :hook ((prog-mode) . lsp-bridge-mode)
  :hook ((prog-mode) . lsp-bridge-semantic-tokens-mode)
  :bind (:map lsp-bridge-mode-map
              ;; ("M-." . find-definitions-with-lsp-bridge)
              ;; ("M-," . mark-power--jump-back)
              ("M-?" . lsp-bridge-find-references)
              ("M-P" . lsp-bridge-popup-documentation-scroll-down)
              ("M-N" . lsp-bridge-popup-documentation-scroll-up))
  :bind (:map acm-mode-map
              ("M-P" . acm-doc-scroll-down)
              ("M-N" . acm-doc-scroll-up))
  :bind (:map lsp-bridge-peek-keymap
              ("M-p" . lsp-bridge-peek-list-prev-line)
              ("M-n" . lsp-bridge-peek-list-next-line)
              ("<return>" . lsp-bridge-peek-jump))
  :custom
  (acm-enable-icon t)
  (acm-enable-yas t)
  (acm-enable-doc t)
  (acm-enable-citre nil)
  (acm-enable-doc-markdown-render t)
  (acm-enable-path t)
  (acm-backend-yas-match-by-trigger-keyword t)
  (acm-enable-tabnine nil)
  (acm-enable-codeium nil)
  (acm-enable-preview t)
  (acm-enable-capf t)
  (acm-backend-search-file-words-enable-fuzzy-match t)
  (acm-enable-lsp-workspace-symbol nil) ;dont show workspace symbol
  (acm-enable-quick-access t)

  (lsp-bridge-enable-log nil)
  (lsp-bridge-enable-diagnostics nil) ;; we use flycheck only
  (lsp-bridge-enable-hover-diagnostic t)
  (lsp-bridge-code-action-enable-popup-menu t)
  (lsp-bridge-find-ref-fallback #'xref-find-references)
  (lsp-bridge-inlay-hint t)
  (lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
  (lsp-bridge-python-lsp-server "ruff")
  (lsp-bridge-python-multi-lsp-server "pyright_ruff")
  (lsp-bridge-multi-lang-server-extension-list
   '(
     (("ts") . "typescript_eslint")
     (("tsx") . "typescriptreact_eslint")
     (("css" "scss" "sass" "less") . "css_emmet")
     (("vue") . "volar_emmet")
     ))

  :config
  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("html") . "html_tailwindcss"))
  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("css") . "css_tailwindcss"))

  (leader-def :keymaps 'lsp-bridge-mode-map
    :infix "c"
    "" '(:ignore t :wk "Code")
    "a"  '(lsp-bridge-code-action :wk "Code actions")
    "e"  '(lsp-bridge-diagnostic-list :wk "Diagnostic list")
    "d" '(lsp-bridge-find-def :wk "Find define")
    "f" '(lsp-bridge-code-format :wk "Format code")
    "i"  '(lsp-bridge-find-impl :wk "Find implementation")
    "k"  '(lsp-bridge-popup-documentation :wk "Find Document")
    "p"  '(lsp-bridge-peek :wk "Peek")
    "r" '(lsp-bridge-rename :wk "Rename")
    "t"  '(lsp-bridge-find-type-def :wk "Find type definition")
    "?" '(lsp-bridge-find-references :wk "Find References")))


(add-hook 'web-mode-hook (lambda ()
                           (setq lsp-bridge-completion-obey-trigger-characters-p nil)
                           (setq lsp-bridge-completion-hide-characters '(":" ";" "(" ")" "[" "]" "{" "}" "," "\"" ">" "()" "{}"))))


(provide 'init-lsp-bridge)


