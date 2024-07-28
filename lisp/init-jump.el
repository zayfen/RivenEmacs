
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
  :init
  :bind ("M-g [" . goto-last-change)
  :commands goto-last-change
  )




(provide 'init-jump)
