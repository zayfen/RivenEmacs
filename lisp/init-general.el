
;; -*- coding: utf-8; lexical-binding: t -*-


(use-package general
  :vc (:fetcher github :repo noctuid/general.el)
  :config
  (setq general-keymap-global-map global-map) ; Set the keymap for general.el as global-map
					; Other configurations specific to your needs...
  
  (general-create-definer leader-def
    :prefix "C-c")

  (general-create-definer local-leader-def
    :prefix "C-c l"))


(provide 'init-general)
