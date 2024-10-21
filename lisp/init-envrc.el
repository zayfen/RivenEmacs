;; -*- coding: utf-8; lexical-binding: t -*-

(use-package envrc
  :vc (:fetcher github :repo "purcell/envrc")
  :hook (after-init . envrc-global-mode))


(provide 'init-envrc)
;;; init-envrc.el ends here
