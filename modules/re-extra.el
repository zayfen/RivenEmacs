;;; re-extra.el --- Some extra functionalities -*- lexical-binding: t; -*-



(use-package crux
  :straight t
  :init
  (+map!
    "fo" #'crux-open-with
    "fU" #'crux-sudo-edit
    "fD" #'crux-delete-file-and-buffer
    "fC" #'crux-copy-file-preserve-attributes
    "id" #'crux-insert-date
    "bo" #'crux-kill-other-buffers))


(provide 're-extra)
