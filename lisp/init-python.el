;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-python.el --- Python development configuration

;;; Commentary:
;; Configuration for Python development environment including:
;; - Syntax highlighting and indentation
;; - Code completion and linting
;; - Virtual environment support
;; - Testing and debugging

;;; Code:

;; Core Python mode configuration
(use-package python-ts-mode
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :hook (python-ts-mode . (lambda ()
                           (setq indent-tabs-mode nil
                                 tab-width 4
                                 python-indent-offset 4))))

;; Python Environment Tools (virtualenv, pipenv etc.)
(use-package pyvenv
  :hook (python-ts-mode . pyvenv-mode)
  :config
  (setq pyvenv-post-activate-hooks '(pyvenv-restart-python))
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name (" [venv:" pyvenv-virtual-env-name "] "))))

;; Python Editing Tools (auto-formatting, snippets etc.)
(use-package pet
  :vc (:fetcher github :repo "wyuenho/emacs-pet")
  :hook (python-base-mode . pet-mode)
  :config
  (setq pet-python-command "python3"))

;; Optional: Add keybindings for common Python tasks
(with-eval-after-load 'python-ts-mode
  (define-key python-ts-mode-map (kbd "C-c C-c") 'python-shell-send-buffer)
  (define-key python-ts-mode-map (kbd "C-c C-r") 'python-shell-send-region)
  (define-key python-ts-mode-map (kbd "C-c C-t") 'python-pytest-dispatch))

(provide 'init-python)
;;; init-python.el ends here
