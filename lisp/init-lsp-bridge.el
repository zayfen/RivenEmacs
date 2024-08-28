;;; -*- coding: utf-8; lexical-binding: t -*-

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :hook
  (markdown-mode . nb/markdown-unhighlight)
  :config
  (defvar nb/current-line '(0 . 0)
    "(start . end) of current line in current buffer")
  (make-variable-buffer-local 'nb/current-line)

  (defun nb/unhide-current-line (limit)
    "Font-lock function"
    (let ((start (max (point) (car nb/current-line)))
          (end (min limit (cdr nb/current-line))))
      (when (< start end)
        (remove-text-properties start end
                                '(invisible t display "" composition ""))
        (goto-char limit)
        t)))

  (defun nb/refontify-on-linemove ()
    "Post-command-hook"
    (let* ((start (line-beginning-position))
           (end (line-beginning-position 2))
           (needs-update (not (equal start (car nb/current-line)))))
      (setq nb/current-line (cons start end))
      (when needs-update
        (font-lock-fontify-block 3))))

  (defun nb/markdown-unhighlight ()
    "Enable markdown concealling"
    (interactive)
    (markdown-toggle-markup-hiding 'toggle)
    (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
    (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))
  :custom-face
  (markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
  (markdown-header-face-1 ((t (:height 1.6  :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-2 ((t (:height 1.4  :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-3 ((t (:height 1.2  :foreground "#D08770" :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-4 ((t (:height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face))))
  (markdown-header-face-5 ((t (:height 1.1  :foreground "#b48ead" :weight bold :inherit markdown-header-face))))
  (markdown-header-face-6 ((t (:height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))))
  :hook
  (markdown-mode . abbrev-mode))

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

(defun find-definitions-with-lsp-bridge ()
  (interactive)
  (if lsp-bridge-mode (lsp-bridge-find-def)
    (call-interactively 'xref-find-definitions)))

(advice-add #'lsp-bridge-find-def :around (lambda (&rest args) (xref--push-markers) (apply args)))
(advice-add #'lsp-bridge-find-impl :around (lambda (&rest args) (xref--push-markers) (apply args)))
(advice-add #'lsp-bridge-find-type-def :around (lambda (&rest args) (xref--push-markers) (apply args)))
(advice-add #'lsp-bridge-peek-jump :around (lambda (&rest args) (xref--push-markers) (apply args)))



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
  :bind (:map lsp-bridge-peek-keymap
              ("M-p" . lsp-bridge-peek-list-prev-line)
              ("M-n" . lsp-bridge-peek-list-next-line)
              ("M-j" . lsp-bridge-peek-jump))
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


(provide 'init-lsp-bridge)
