;; -*- coding: utf-8; lexical-binding: t -*-

;; install expand-region

(use-package expand-region
  :vc (:fetcher github :repo magnars/expand-region.el)
  :bind ("C-=" . er/expand-region))

(use-package iedit
  :demand t
  :vc (:fetcher github :repo victorhge/iedit)
  :config
  ;; Define a new face for iedit occurrence highlighting
  (defface iedit-occurrence
    '((t (:background "#1A8899" :foreground "black")))
    "Face for iedit occurrence highlighting.")
  (add-hook 'iedit-mode-hook
            (lambda ()
              (set-face-attribute 'iedit-occurrence nil :background "#1A8899" :foreground "black")))
  )

(use-package avy
  :vc (:fetcher github :repo abo-abo/avy)
  :bind ("C-'" . avy-goto-char-2)
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-c C-j") 'avy-resume))

(use-package link-hint
  :ensure t
  :bind
  ("C-|" . link-hint-open-link))

(use-package sudo-edit)

(use-package visual-regexp
  :bind (("C-c r" . #'vr/replace)))


(use-package color-rg
  :no-require
  :vc (:fetcher github :repo manateelazycat/color-rg)
  :init
  (add-to-list 'load-path (concat repo-dir "color-rg"))
  :commands (color-rg-search-input-in-project color-rg-search-symbol-in-project color-rg-search-input-in-current-file color-rg-search-symbol-in-current-file)
  )

(use-package blink-search
  :vc (:fetcher github :repo manateelazycat/blink-search)
  :init
  (setq blink-search-browser-function
        (if (display-graphic-p)
            #'xwidget-webkit-browse-url
          #'eww)))


(use-package symbol-overlay
  :ensure t
  :init
  (setq symbol-overlay-map (make-sparse-keymap))
  (setq re/symbol-overlay-keymap (make-sparse-keymap))
  (define-key re/symbol-overlay-keymap (kbd ".") 'symbol-overlay-put)
  (define-key re/symbol-overlay-keymap (kbd "n") 'symbol-overlay-jump-next)
  (define-key re/symbol-overlay-keymap (kbd "p") 'symbol-overlay-jump-prev)
  (define-key re/symbol-overlay-keymap (kbd "w") 'symbol-overlay-save-symbol)
  (define-key re/symbol-overlay-keymap (kbd "t") 'symbol-overlay-toggle-in-scope)
  (define-key re/symbol-overlay-keymap (kbd "e") 'symbol-overlay-echo-mark)
  (define-key re/symbol-overlay-keymap (kbd "d") 'symbol-overlay-jump-to-definition)
  (define-key re/symbol-overlay-keymap (kbd "s") 'symbol-overlay-isearch-literally)
  (define-key re/symbol-overlay-keymap (kbd "q") 'symbol-overlay-query-replace)
  (define-key re/symbol-overlay-keymap (kbd "r") 'symbol-overlay-rename)
  (global-set-key (kbd "M-o") re/symbol-overlay-keymap)
  (global-set-key (kbd "M-n") 'symbol-overlay-jump-next)
  (global-set-key (kbd "M-p") 'symbol-overlay-jump-prev)
  :hook ((prog-mode text-mode) . symbol-overlay-mode))


(use-package hl-todo
  :vc (:fetcher github :repo tarsius/hl-todo)
  :config
  (hl-todo-mode))

(use-package dirvish
  :ensure t
  :custom
  (dirvish-attributes '(subtree-state all-the-icons file-size vc-state git-msg))
  (dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  (dirvish-side-width 30)
  (dirvish-fd-default-dir "~/")
  (dirvish-use-header-line t) ; 'global make header line span all panes
  (dirvish-use-mode-line t)
)

(provide 'init-editor)
