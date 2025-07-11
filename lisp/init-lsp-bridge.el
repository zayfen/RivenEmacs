;;; -*- coding: utf-8; lexical-binding: t -*-

(use-package yasnippet
  :ensure t
  :custom
  (yas-snippet-dir "~/.emacs.d/snippets")
  :hook ((prog-mode LaTeX-mode org-mode markdown-mode) . yas-minor-mode-on)
  :bind
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

(defun lsp-bridge-find-def-return-ex ()
  "Try to go back via \"lsp-bridge-find-def-return\", if error, use \"xref-go-back\" instead."
  (interactive)
  (condition-case err
      (call-interactively 'lsp-bridge-find-def-return)
    (error
     (call-interactively 'xref-go-back))))

(defun lsp-bridge-find-def-ex()
  "Try to find define via \"lsp-bridge-find-def-return\", if error, use \"xref-find-defination\" instead."
  (interactive)
  (condition-case err
      (call-interactively 'lsp-bridge-find-def)
    (error
     (call-interactively 'xref-find-definitions))))

(defun lsp-bridge-find-def-fallback-ex (&rest _)
  "Define fallback function of lsp-bridge-find-def-fallback"
  (call-interactively 'xref-find-definitions))

(defun lsp-bridge-find-ref-fallback-ex (&rest _)
  "Define fallback function of lsp-bridge-find-def-fallback"
  (setq xref-prompt-for-identifier nil)
  (call-interactively 'xref-find-references))

(use-package lsp-bridge
  :vc (:url "https://github.com/manateelazycat/lsp-bridge.git" :branch "master" :rev "3b37a04bd1b6bbcdc2b0ad7a5c388ad027eb7a25")
  :hook ((prog-mode) . lsp-bridge-mode)
  :hook ((prog-mode) . lsp-bridge-semantic-tokens-mode)
  :bind (:map lsp-bridge-mode-map
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
  (acm-enable-doc-markdown-render t)
  (acm-enable-path t)
  (acm-backend-yas-match-by-trigger-keyword t)
  (acm-enable-preview t)
  (acm-enable-capf t)
  (acm-backend-search-file-words-enable-fuzzy-match t)
  (acm-enable-lsp-workspace-symbol nil) ;dont show workspace symbol
  (acm-enable-quick-access t)
  (acm-backend-search-file-words-candidate-min-length 3)

  (lsp-bridge-enable-log nil)
  (lsp-bridge-enable-diagnostics nil)
  (lsp-bridge-enable-hover-diagnostic t)
  (lsp-bridge-code-action-enable-popup-menu nil)
  (lsp-bridge-find-def-fallback-function #'xref-find-definitions)
  (lsp-bridge-find-ref-fallback-function #'lsp-bridge-find-ref-fallback-ex)
  (lsp-bridge-inlay-hint t)
  (lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
  (lsp-bridge-python-lsp-server "ruff")
  (lsp-bridge-python-multi-lsp-server "pyright_ruff")
  (lsp-bridge-multi-lang-server-extension-list
   '(
     (("css" "scss" "sass" "less") . "css_emmet")
     ("vue" . "volar_emmet")
     ("tsx" . "typescriptreact_eslint")
     ("ts" . "typescript_eslint")))

  :config
  (define-key acm-mode-map (kbd "C-m") nil)
  (define-key acm-mode-map (kbd "<return>") nil)
  (keymap-global-set "M-." #'lsp-bridge-find-def-ex)
  (keymap-global-set "M-," #'lsp-bridge-find-def-return-ex)
  (keymap-global-set "M-?" #'lsp-bridge-find-references)

  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("html") . "html_tailwindcss"))
  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("css" "scss" "sass" "less") . "css_tailwindcss"))
)


(add-hook 'web-mode-hook (lambda ()
                           (setq lsp-bridge-completion-obey-trigger-characters-p nil)
                           (setq lsp-bridge-completion-hide-characters '(":" ";" "(" ")" "[" "]" "{" "}" "," "\"" ">" "()" "{}"))))


(provide 'init-lsp-bridge)
