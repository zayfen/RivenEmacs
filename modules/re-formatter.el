;;; re-formatter.el --- Config for code formatting   -*- lexical-binding: t; -*-


;;; Code:

(use-package apheleia
  :straight t
  :ensure t
  :hook (prog-mode . apheleia-mode))

(use-package apheleia-formatters
  :config
  (add-to-list 'apheleia-formatters '(cmake-format . ("cmake-format")))
  (dolist (alist '((cmake-mode . cmake-format)
                   (cmake-ts-mode . cmake-format)
                   (lisp-data-mode . lisp-indent)
                   (sh-mode . shfmt)
                   (emacs-lisp-mode . lisp-indent)))
    (add-to-list 'apheleia-mode-alist alist)))


(use-package editorconfig
  :straight t
  :ensure t
  :hook (prog-mode . editorconfig-mode)
  :init
  (+map!
    "fc" '(editorconfig-find-current-editorconfig :wk "Find current EditorConfig")
    "bf" #'editorconfig-format-buffer)
  :config (editorconfig-mode 1))

(use-package clang-format
  :straight t
  :init
  (+map! :keymaps '(c-mode-map c++-mode-map cuda-mode-map scad-mode-map)
    "cfc" #'clang-format-buffer))


(provide 're-formatter)
