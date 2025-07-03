;;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-fold.el --- Config for code fold -*- lexical-binding: t; -*-
;;; Code: fold feature

(use-package treesit-fold
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold")
  :commands (treesit-fold-toggle)
  :bind ("M-i" . treesit-fold-toggle)
  :config
  (defun my-enable-hs-if-treesit-unavailable ()
    (unless (car (treesit-parser-list))
      (hs-minor-mode)
      (define-key hs-minor-mode-map (kbd "M-i") 'hs-toggle-hiding)))
  (add-hook 'prog-mode-hook #'my-enable-hs-if-treesit-unavailable))



;; (use-package treesit-fold-indicators
;;   :vc (:url "https://github.com/emacs-tree-sitter/ts-fold")")

(use-package hideif
  :init
  (dolist (h '(c++-mode-hook c++-ts-mode-hook c-mode-hook c-ts-mode-hook cuda-mode-hook))
      (add-hook h #'hide-ifdef-mode))
  :custom
  (hide-ifdef-shadow t)
  (hide-ifdef-initially t))

(provide 'init-fold)
