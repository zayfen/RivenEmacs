;;; modules/re-autosave.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 zayfen

;; Author zayfen (zhangyunfeng0101@gmail.com)

;;; Code

(use-package auto-save
  :ensure t
  :straight (auto-save
             :type git
             :host github
             :repo "manateelazycat/auto-save"
             :files ("*" (:exclude ".git"))
             :build nil)
  :init
  (add-to-list 'load-path (straight--repos-dir "auto-save"))
  :config
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace nil)
  (setq auto-save-disable-predicates
      '((lambda ()
      (string-suffix-p
      "gpg"
      (file-name-extension (buffer-name)) t)))))

(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)
(setq auto-save-delete-trailing-whitespace nil)
