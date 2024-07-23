

(use-package general
  :vc (:fetcher github :repo noctuid/general.el)
  :config
  (setq general-keymap-global-map global-map) ; Set the keymap for general.el as global-map
  ; Other configurations specific to your needs...
)



(provide 'init-general)
