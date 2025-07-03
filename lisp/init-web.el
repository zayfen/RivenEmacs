;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-web.el --- web config

(use-package emmet-mode
  :ensure t
  :defer t)

(defun my-web-mode-hook ()
  "Hooks for Web-mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-style-padding 0)
  (setq web-mode-script-padding 0))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.ejs\\'")
  :commands web-mode
  :config
  (setq web-mode-enable-auto-closing t) ;)
  (setq web-mode-enable-auto-quoting nil) ; this fixes the quote problem I mentioned
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  (setq tab-width 2)
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'web-mode-hook
            (lambda()
              (local-unset-key (kbd "C-c C-l")))))

;; Define vue-mode
(define-derived-mode vue-mode web-mode "Vue"
  "Major mode for Vue.js files.")

;; Associate .vue files with vue-mode
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-hook 'vue-mode-hook 'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-ts-mode))

;; config indent
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq js-switch-indent-offset 2)
(setq typescript-ts-mode-indent-offset 2)

(provide 'init-web)
