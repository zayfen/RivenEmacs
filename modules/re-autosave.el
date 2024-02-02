;;; modules/re-autosave.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 zayfen

;; Author zayfen (zhangyunfeng0101@gmail.com)

;;; Code:

(setq auto-save-default nil)

;; (use-package auto-save
;;   :ensure t
;;   :straight (auto-save
;;              :type git
;;              :host github
;;              :repo "manateelazycat/auto-save"
;;              :files ("*" (:exclude ".git"))
;;              :build nil)
;;   :init
;;   (add-to-list 'load-path (straight--repos-dir "auto-save"))
;;   :custom
;;   (auto-save-idle 2)
;;   :config
;;   (setq auto-save-disable-predicates
;;         '((lambda ()
;;             (string-suffix-p
;;              "gpg"
;;              (file-name-extension (buffer-name)) t)))))

;; (require 'auto-save)
;; (auto-save-enable)
;; (setq auto-save-silent t)
;; (setq auto-save-delete-trailing-whitespace nil)


(use-package super-save
  :ensure t
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

