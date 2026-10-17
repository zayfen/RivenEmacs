;; -*- coding: utf-8; lexical-binding: t -*-

;; Enable direnv integration immediately instead of relying on an
;; `after-init-hook' registration made from inside `after-init-hook' (this
;; module is itself loaded from `after-init-hook' at depth -20, so such a
;; registration would never run).
(use-package envrc
  :vc (:url "https://github.com/purcell/envrc")
  :if (executable-find "direnv")
  :demand t
  :config
  (envrc-global-mode 1))


(provide 'init-envrc)
;;; init-envrc.el ends here
