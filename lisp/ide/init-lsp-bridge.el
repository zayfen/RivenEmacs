;;; -*- coding: utf-8; lexical-binding: t -*-

(eval-and-compile
  (let ((current-file (or load-file-name byte-compile-current-file buffer-file-name)))
    (when current-file
      (add-to-list 'load-path (file-name-directory current-file)))))

(require 'init-config)

(use-package yasnippet
  :ensure t
  :custom
  (yas-snippet-dir rivenEmacs-snippets-dir)
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


(defun lsp-bridge-find-ref-fallback-ex (&rest _)
  "Define fallback function of lsp-bridge-find-def-fallback"
  (setq xref-prompt-for-identifier nil)
  (call-interactively 'xref-find-references))


(defun riven/lsp-bridge-semantic-tokens-maybe ()
  "Enable semantic tokens only when the function exists." 
  (when (fboundp 'lsp-bridge-semantic-tokens-mode)
    (lsp-bridge-semantic-tokens-mode 1)))

(use-package lsp-bridge
  :vc (:url "https://github.com/manateelazycat/lsp-bridge" :branch "master")
  :init
  (dolist (mode rivenEmacs-lsp-modes)
    (add-hook (intern (format "%s-hook" mode)) #'lsp-bridge-mode)
    (add-hook (intern (format "%s-hook" mode)) #'riven/lsp-bridge-semantic-tokens-maybe))
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
  (acm-backend-search-file-words-enable-fuzzy-match t)
  (acm-enable-lsp-workspace-symbol nil) ;dont show workspace symbol
  (acm-enable-quick-access t)
  (acm-backend-search-file-words-candidate-min-length 3)

  (lsp-bridge-enable-log nil)
  (lsp-bridge-enable-diagnostics nil)
  (lsp-bridge-code-action-enable-popup-menu nil)
  (lsp-bridge-find-def-fallback-function #'xref-find-definitions)
  (lsp-bridge-find-ref-fallback-function #'lsp-bridge-find-ref-fallback-ex)
  (lsp-bridge-inlay-hint t)
  (lsp-bridge-python-lsp-server "ruff")
  (lsp-bridge-python-multi-lsp-server "pyright_ruff")
  (lsp-bridge-multi-lang-server-extension-list
   '(
     (("css" "scss" "sass" "less") . "css_emmet")
     ("tsx" . "typescriptreact_eslint")
     ("ts" . "typescript_eslint")
     ("vue" . "volar_vtsls")))

  :config
  (define-key acm-mode-map (kbd "C-m") nil)
  (define-key acm-mode-map (kbd "<return>") nil)
  (keymap-global-set "M-." #'lsp-bridge-find-def-ex)
  (keymap-global-set "M-," #'lsp-bridge-find-def-return-ex)
  (keymap-global-set "M-?" #'lsp-bridge-find-references)

  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("html") . "html_tailwindcss"))
  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("css" "scss" "sass" "less") . "css_tailwindcss"))

  ;; Custom mode line display
  (defun lsp-bridge--mode-line-format ()
    "Compose the LSP-bridge's mode-line."
    (setq-local mode-face
                (if (lsp-bridge-process-live-p)
                    'lsp-bridge-alive-mode-line
                  'lsp-bridge-kill-mode-line))
    (when lsp-bridge-server
      (propertize "LSPB" 'face mode-face)))  ;; Use "LSPB" instead of "lsp-bridge"

  (when lsp-bridge-enable-mode-line
    ;; Remove existing lsp-bridge-mode entry
    (setq mode-line-misc-info (assq-delete-all 'lsp-bridge-mode mode-line-misc-info))
    ;; Add new entry
    (add-to-list 'mode-line-misc-info
                 `(lsp-bridge-mode ("" lsp-bridge--mode-line-format " "))))
  )


(add-hook 'web-mode-hook (lambda ()
                           (setq lsp-bridge-completion-obey-trigger-characters-p nil)
                           (setq lsp-bridge-completion-hide-characters '(":" ";" "(" ")" "[" "]" "{" "}" "," "\"" ">" "()" "{}"))))


(provide 'init-lsp-bridge)
;; init-lsp-bridge.el ends here.
