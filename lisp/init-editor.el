;; -*- coding: utf-8; lexical-binding: t -*-

;; install expand-region

(use-package expand-region
  :vc (:fetcher github :repo magnars/expand-region.el)
  :bind ("C-=" . er/expand-region))

(use-package iedit
  :vc (:fetcher github :repo victorhge/iedit))

(use-package avy
  :vc (:fetcher github :repo abo-abo/avy)
  :bind ("C-'" . avy-goto-char-2)
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-c C-j") 'avy-resume))

(use-package link-hint
  :ensure t
  :bind
  ("C-|" . link-hint-open-link))

(use-package sudo-edit)

(use-package visual-regexp
  :bind (("C-c r" . #'vr/replace)))


(provide 'init-editor)
