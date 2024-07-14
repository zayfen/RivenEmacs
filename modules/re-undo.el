;;; re-undo.el --- Undo -*- lexical-binding: t; -*-



;; Visual Undo
(use-package vundo
  :straight t
  :init
  (+map! "ou" #'vundo)
  :custom
  (vundo-compact-display t)
  (vundo-window-max-height 8)
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package undo-fu
  :straight t
  :after rivenemacs-loaded
  :demand t
  :config
  (global-set-key (kbd "M-_") 'undo-redo)
  (global-set-key [remap undo-redo] #'undo-fu-only-redo)
  (global-set-key [remap undo] #'undo-fu-only-undo))

(use-package undo-fu-session
  :straight t
  :after undo-fu
  :demand t
  :custom
  (undo-fu-session-compression (if (executable-find "zstd") 'zst 'gz))
  (undo-fu-session-directory (concat rivenemacs-local-dir "undo-fu-session/"))
  :config
  (global-undo-fu-session-mode 1))


(provide 're-undo)
