
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


(use-package dumb-jump
  :vc (:fetcher github :repo "jacktasia/dumb-jump")
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate -100)
  :after xref
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-disable-obsolete-warnings t))

(provide 'init-jump)
