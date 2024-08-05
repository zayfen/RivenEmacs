;;; -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2023 zayfen

;; Author zayfen (zhangyunfeng0101@gmail.com)

;;; Code:


(use-package super-save
  :init
  (setq auto-save-default nil)
  :hook ((prog-mode . super-save-mode)
         (text-mode . super-save-mode))
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 2)
  (super-save-silent t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  (super-save-exclude '(".gpg"))
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-hook-triggers 'find-file-hook))



(provide 'init-autosave)
