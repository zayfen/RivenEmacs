;; -*- coding: utf-8; lexical-binding: t -*-

(use-package vundo
  :commands (vundo)
  :bind ("M-_" . vundo)
  :config
  (setq vundo-compact-display t))


(use-package undo-fu
  :config
  (global-set-key [remap undo-redo] #'undo-fu-only-redo)
  (global-set-key [remap undo] #'undo-fu-only-undo))

(use-package undo-fu-session
  :after undo-fu
  :custom
  (undo-fu-session-compression (if (executable-find "zstd") 'zst 'gz))
  (undo-fu-session-directory (concat local-dir "undo-fu-session/"))
  :config
  (global-undo-fu-session-mode 1))



(provide 'init-undo)
