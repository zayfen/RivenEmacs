;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-python.el --- Python development config


(use-package python-ts-mode
  :ensure nil
  :mode ("\\.py$\'" . python-ts-mode))

(use-package pet
  :vc (:fetcher github :repo "wyuenho/emacs-pet")
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(provide 'init-python)
 ;;; init-python.el ends here
