
;; -*- coding: utf-8; lexical-binding: t -*-



;; install ace-window
(use-package ace-window
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(set-frame-parameter (selected-frame)
                     'internal-border-width 0)




(provide 'init-jump)

