;; -*- coding: utf-8; lexical-binding: t -*-


(use-package which-key
  :demand t
  :custom
  (which-key-idle-delay 1.0)
  (which-key-idle-secondary-delay nil)
  (which-key-ellipsis "..")
  (which-key-separator " → " )
  (which-key-unicode-correction 3)
  (which-key-prefix-prefix "+")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-min-display-lines 3)
  (which-key-max-display-columns nil)
  ;; Allow a key binding to be modified by multiple rules in
  ;; `which-key-replacement-alist'
  (which-key-allow-multiple-replacements t)

  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(provide 'init-which-key)
