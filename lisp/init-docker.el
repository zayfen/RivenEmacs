;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-dockerfile.el --- Dockerfile configuration



(use-package dockerfile-mode)

(use-package docker
  :bind ("C-c o d" . docker))

(provide 'init-dockerfile)
