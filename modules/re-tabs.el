;;; re-tabs --- tabs for buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; NOTE: tabs for buffers, quickly jump between tabs

;;; Code:

(use-package centaur-tabs
  :ensure
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("M-g <" . centaur-tabs-backward)
  ("M-g >" . centaur-tabs-forward))

(provide 're-tabs)

;;; re-tabs.el ends here
