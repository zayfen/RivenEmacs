;; -*- coding: utf-8; lexical-binding: t -*-

;; install expand-region

(use-package expand-region
  :vc (:url "https://github.com/magnars/expand-region.el")
  :bind ("C-=" . er/expand-region))

(use-package iedit
  :demand t
  :vc (:url "https://github.com/victorhge/iedit")
  :bind
  (("C-;" . iedit-mode)
   :map iedit-mode-occurrence-keymap
        (("M-n" . iedit-next-occurrence)
         ("M-p" . iedit-prev-occurrence))))

(use-package avy
  :vc (:url "https://github.com/abo-abo/avy")
  :commands (avy-goto-char-2)
  :config
  (avy-setup-default))

(use-package link-hint
  :ensure t
  :commands (link-hint-open-link))

(use-package sudo-edit)

(use-package visual-regexp
  :commands (vr/replace))


(use-package color-rg
  :no-require
  :vc (:url "https://github.com/manateelazycat/color-rg")
  :init
  (add-to-list 'load-path (concat repo-dir "color-rg"))
  :commands (color-rg-search-input-in-project color-rg-search-symbol-in-project color-rg-search-input-in-current-file color-rg-search-symbol-in-current-file))

(use-package deadgrep
  :vc (:url "https://github.com/Wilfred/deadgrep" :branch "master")
  :commands (deadgrep))


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
  (global-set-key (kbd "M-s o") re/symbol-overlay-keymap)
  (global-set-key (kbd "M-n") 'symbol-overlay-jump-next)
  (global-set-key (kbd "M-p") 'symbol-overlay-jump-prev)
  :hook ((prog-mode text-mode) . symbol-overlay-mode))


(use-package hl-todo
  :vc (:url "https://github.com/tarsius/hl-todo")
  :hook (prog-mode . hl-todo-mode))

(defun riven/goggles-mode-maybe ()
  "Enable `goggles-mode` only when the package is available."
  (when (fboundp 'goggles-mode)
    (goggles-mode 1)))

(use-package goggles
  :vc (:url "https://github.com/minad/goggles" :branch "main")
  :hook ((prog-mode text-mode) . riven/goggles-mode-maybe)
  :config
  (setq-default goggles-pulse t))

(defun riven/jinx-available-p ()
  "Return non-nil when the Enchant backend is available for jinx."
  (or (executable-find "enchant-2")
      (executable-find "enchant-lsmod-2")
      (executable-find "enchant")))

(unless (riven/jinx-available-p)
  (message "jinx disabled: install enchant + hunspell to enable spell checking."))

(use-package jinx
  :if (riven/jinx-available-p)
  :hook ((text-mode . jinx-mode)
         (prog-mode . jinx-mode))
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :custom
  (jinx-languages "en_US"))

(provide 'init-editor)
