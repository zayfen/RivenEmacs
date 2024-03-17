;;; re-tabs --- tabs for buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; NOTE: tabs for buffers, quickly jump between tabs

;;; Code:

(use-package centaur-tabs
  :straight t
  :demand
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-gray-out-icons 'buffer)
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-modified-marker t)
  :bind
  ("M-g <" . centaur-tabs-backward)
  ("M-g >" . centaur-tabs-forward))

(provide 're-tabs)

;;; re-tabs.el ends here
