;; -*- coding: utf-8; lexical-binding: t -*-

(use-package envrc
  :vc (:url "https://github.com/purcell/envrc")
  :if (executable-find "direnv")
  :hook (after-init . envrc-global-mode))


(provide 'init-envrc)
;;; init-envrc.el ends here
