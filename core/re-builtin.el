;;; re-builtin.el --- Customization of some of Emacs' builtin libraries -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


(use-package transient
  :straight (:type built-in)
  :config
  ;; Map ESC and q to quit transient
  (define-key transient-map [escape]  #'transient-quit-one)
  (define-key transient-map (kbd "q") #'transient-quit-one))

(use-package map
  :straight (:type built-in))

(use-package let-alist
  :straight (:type built-in))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))


(provide 're-builtin)
