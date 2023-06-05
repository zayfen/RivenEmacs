;;; re-formatter.el --- Config for code formatting   -*- lexical-binding: t; -*-




(use-package apheleia
  :straight t
  )

(use-package apheleia-formatters
  :config
  (add-to-list 'apheleia-formatters '(cmake-format . ("cmake-format")))
  ;; TEMP: Use the `tab-width' value for `shfmt' formatting. Delete this hack if
  ;; this PR github.com/radian-software/apheleia/pull/179 gets merged.
  (+alist-set 'shfmt '("shfmt" "-i" (number-to-string tab-width)) apheleia-formatters)
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