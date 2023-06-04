;;; re-fold.el --- Config for code indent -*- lexical-binding: t; -*-

;; code fold
(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :init
  (+map! "@ TAB" #'ts-fold-toggle))

(use-package ts-fold-indicators
  :straight (ts-fold-indicators :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :init (add-hook 'tree-sitter-after-on-hook #'ts-fold-indicators-mode))

(use-package hideif
  :straight (:type built-in)
  :init
  (unless (memq 're-lsp rivenemacs-modules)
    (dolist (h '(c++-mode-hook c++-ts-mode-hook c-mode-hook c-ts-mode-hook cuda-mode-hook))
      (add-hook h #'hide-ifdef-mode)))
  :custom
  (hide-ifdef-shadow t)
  (hide-ifdef-initially t))

(provide 're-fold)
