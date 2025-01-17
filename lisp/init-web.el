;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-web.el --- web config

(use-package emmet-mode
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.ejs\\'")
  :commands web-mode
  :config
  (setq web-mode-enable-auto-closing t) ;)
  (setq web-mode-enable-auto-quoting nil) ; this fixes the quote problem I mentioned

  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2))
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  (setq tab-width 2)
  (add-hook 'web-mode-hook  'emmet-mode)
  ;; C-l C-l is lookup function
  (add-hook 'web-mode-hook
            (lambda()
              (local-unset-key (kbd "C-c C-l")))))

;; Define vue-mode
(define-derived-mode vue-mode web-mode "Vue"
  "Major mode for Vue.js files.")

;; Associate .vue files with vue-mode
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

;; Customize web-mode settings for vue-mode
(defun my-vue-mode-hook ()
  "Hooks for Vue.js files."
  ;; Set indentation
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  ;; Additional configurations can go here
)
(add-hook 'vue-mode-hook 'my-vue-mode-hook)


(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-ts-mode))

(setq js-indent-level 2)
(setq typescript-ts-mode-indent-offset 2)

;; https://github.com/llemaitre19/jtsx
(use-package jtsx
  :vc (:fetcher github :repo "llemaitre19/jtsx")
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode))
  :commands jtsx-install-treesit-language
  :custom
  ;; Optional customizations
  (js-indent-level 2)
  (typescript-ts-mode-indent-offset 2)
  (jtsx-switch-indent-offset 2)
  :config
  (flycheck-add-mode 'javascript-eslint 'jtsx-tsx-mode)
  (flycheck-add-mode 'javascript-eslint 'jtsx-jsx-mode)
  (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (define-key mode-map (kbd "C-c e j") 'jtsx-jump-jsx-element-tag-dwim)
    (define-key mode-map (kbd "C-c e [") 'jtsx-jump-jsx-opening-tag)
    (define-key mode-map (kbd "C-c e ]") 'jtsx-jump-jsx-closing-tag)
    (define-key mode-map (kbd "C-c e r") 'jtsx-rename-jsx-element)
    (define-key mode-map (kbd "C-c e f") 'jtsx-move-jsx-element-tag-forward)
    (define-key mode-map (kbd "C-c e b") 'jtsx-move-jsx-element-tag-backward)
    ;; (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
    ;; (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
    ;; (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
    ;; (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
    (define-key mode-map (kbd "C-c e w") 'jtsx-wrap-in-jsx-element)
    (define-key mode-map (kbd "C-c e u") 'jtsx-unwrap-jsx)
    (define-key mode-map (kbd "C-c e d") 'jtsx-delete-jsx-node)
    (define-key mode-map (kbd "C-c e t") 'jtsx-toggle-jsx-attributes-orientation)
    ;;(define-key mode-map (kbd "M-i") 'hs-toggle-hiding)
    ;; (define-key mode-map (kbd "C-c e h") 'jtsx-rearrange-jsx-attributes-horizontally)
    ;; (define-key mode-map (kbd "C-c e v") 'jtsx-rearrange-jsx-attributes-vertically)
    )

  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map)
  ;; add this mode to treesit-fold-range-alist
  (require 'treesit-fold)
  (add-to-list 'treesit-fold-range-alist
               '(jtsx-tsx-mode . ,(treesit-fold-parsers-typescript))))

;; config css-mode
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq js-switch-indent-offset 2)


(provide 'init-web)
