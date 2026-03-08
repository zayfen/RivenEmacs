;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-python.el --- Python development configuration

;;; Commentary:
;; Configuration for Python development environment including:
;; - Syntax highlighting and indentation
;; - Code completion and linting
;; - Virtual environment support
;; - Testing and debugging

;;; Code:

(defun riven/python-mode-setup ()
  "Basic Python editing setup and Ruff lint backend."
  (define-key python-base-mode-map (kbd "\C-c\C-l") nil)
  (setq indent-tabs-mode nil
        tab-width 4
        python-indent-offset 4)
  (when (executable-find "ruff")
    ;; Use Ruff as the python.el Flymake backend instead of pyflakes.
    (setq-local python-flymake-command
                '("ruff" "check" "--stdin-filename" "stdin" "-"))))

;; Core Python mode configuration
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :hook ((python-base-mode . riven/python-mode-setup)))

;; Python Environment Tools (virtualenv, pipenv etc.)
(use-package pyvenv
  :hook (python-base-mode . pyvenv-mode)
  :config
  (setq pyvenv-post-activate-hooks '(pyvenv-restart-python))
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name (" [venv:" pyvenv-virtual-env-name "] "))))

;; Python Editing Tools (auto-formatting, snippets etc.)
(use-package pet
  :vc (:url "https://github.com/wyuenho/emacs-pet")
  :hook (python-base-mode . pet-mode)
  :config
  (setq pet-python-command "python3"))

;; Optional: Add keybindings for common Python tasks
(with-eval-after-load 'python
  (define-key python-base-mode-map (kbd "C-c l c") 'python-shell-send-buffer)
  (define-key python-base-mode-map (kbd "C-c l r") 'python-shell-send-region)
  (define-key python-base-mode-map (kbd "C-c l t") 'python-pytest-dispatch)
  (define-key python-base-mode-map (kbd "\C-c\C-l") nil)
  (define-key python-ts-mode-map (kbd "\C-c\C-l") nil))

(provide 'init-python)
;;; init-python.el ends here
