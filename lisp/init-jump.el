
;; -*- coding: utf-8; lexical-binding: t -*-



;; install ace-window
(use-package ace-window
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(set-frame-parameter (selected-frame)
                     'internal-border-width 0)



(use-package goto-last-change
  :bind ("M-[" . goto-last-change)
  :commands goto-last-change)

(use-package dogears
  :hook (prog-mode . dogears-mode)
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar))
  :config
  (setq dogears-functions '(windmove-do-window-select
                            +goto-file-at-point
                            pop-to-mark-command
                            pop-global-mark
                            goto-last-change
                            xref-go-back
                            xref-find-definitions
                            embark-dwim
                            xref-find-references
                            lsp-bridge-find-def
                            lsp-bridge-find-type-def
                            lsp-bridge-find-impl
                            lsp-bridge-find-references)))


(provide 'init-jump)
