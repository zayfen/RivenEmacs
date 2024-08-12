;;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-fold.el --- Config for code fold -*- lexical-binding: t; -*-

;; use treesitter-context instead
;; (use-package ts-fold
;;   :vc (:fetcher github :repo emacs-tree-sitter/ts-fold)
;;   :commands (ts-fold-toggle)
;;   :bind ("M-i" . ts-fold-toggle)
;;   :init
;;   )

;; (use-package ts-fold-indicators
;;   :vc (:fetcher github :repo "emacs-tree-sitter/ts-fold")
;;   :init (add-hook 'tree-sitter-after-on-hook #'ts-fold-indicators-mode))

(use-package hideif
  :init
  (dolist (h '(c++-mode-hook c++-ts-mode-hook c-mode-hook c-ts-mode-hook cuda-mode-hook))
      (add-hook h #'hide-ifdef-mode))
  :custom
  (hide-ifdef-shadow t)
  (hide-ifdef-initially t))


(use-package hideshow
  :ensure nil
  :bind (:map hs-minor-mode-map
              ("M-i" . hs-toggle-hiding)
              ("M-[" . hs-hide-all)
              ("M-]" . hs-show-all)))



(use-package treesitter-context
  :vc (:fetcher github :repo "zbelial/treesitter-context.el")
  :bind ("M-i" . treesitter-context-fold-toggle)
  :config
  (add-hook 'typescript-ts-mode-hook #'treesitter-context-mode)
  (add-hook 'typescript-ts-mode-hook #'treesitter-context-focus-mode)
  (add-hook 'tsx-ts-mode-hook #'treesitter-context-mode)
  (add-hook 'tsx-ts-mode-hook #'treesitter-context-focus-mode))


(provide 'init-fold)
