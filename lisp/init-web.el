;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-web.el --- web config




(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.handlebars?\\'" . web-mode))
  (setq web-mode-enable-auto-closing t) ;)
  (setq web-mode-enable-auto-quoting t) ; this fixes the quote problem I mentioned

  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2))
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  (setq tab-width 2)
  (add-hook 'web-mode-hook  'emmet-mode))

(add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-ts-mode))




(provide 'init-web)
