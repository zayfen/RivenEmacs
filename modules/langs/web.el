(use-package apheleia
  :straight t
  :commands apheleia-mode
  :config
  (push '(tsx-ts-mode . prettier-typescript) apheleia-mode-alist)
  (setf (alist-get 'prettier-json apheleia-formatters)
        '(npx "prettier" "--stdin-filepath" filepath)))

(use-package emmet-mode
  :straight t
  :preface (defvar emmet-mode-keymap (make-sparse-keymap))
  :hook (css-mode web-mode html-mode haml-mode nxml-mode rjsx-mode tsx-ts-mode reason-mode)
  :config
  (when (require 'yasnippet nil t)
    (add-hook 'emmet-mode-hook #'yas-minor-mode-on))
  (setq emmet-move-cursor-between-quotes t)
  (add-hook 'tsx-ts-mode-hook 'emmet-expand-jsx-className)
  (+map! :keymaps 'emmet-mode-keymap
        "v TAB" #'emmet-wrap-with-markup
        "TAB" #'+web/indent-or-yas-or-emmet-expand
        "M-E" #'emmet-expand-line))

(use-package web-mode
  :straight t
  :hook (web-mode . +javascript-add-npm-path-h)
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.eex\\'" . web-mode)
         ("\\.html\\.heex\\'" . web-mode)
         ("\\.html\\.tera\\'" . web-mode)
         ("\\.html\\.jinja\\'" . web-mode)
         ("\\.html\\.j2\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-indentation nil)
  (web-mode-enable-auto-pairing nil)
  (web-mode-engines-alist '(("django" . "\\.html\\.tera\\'"))))

(use-package css-ts-mode
  :straight (:type built-in)
  :mode (("\\.css\\'" . css-ts-mode)
         ("\\.scss\\'" . css-ts-mode))
  :hook (css-ts-mode . +javascript-add-npm-path-h)
  :hook (css-ts-mode . apheleia-mode)
  :custom (css-indent-offset 2))

(use-package json-ts-mode
  :straight (:type built-in)
  :mode (("\\.json\\'" . json-ts-mode))
  :hook (json-ts-mode . +javascript-add-npm-path-h)
  :hook (json-ts-mode . apheleia-mode))

(use-package js-ts-mode
  :straight (:type built-in)
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))
  :hook (js-ts-mode . +javascript-add-npm-path-h)
  :hook (js-ts-mode . apheleia-mode)
  :custom (js-indent-level 2))

(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode (("\\.ts\\'" . typescript-ts-mode))
  :hook (typescript-ts-mode . +javascript-add-npm-path-h)
  :hook (typescript-ts-mode . apheleia-mode)
  :custom (typescript-ts-mode-indent-offset 2))


(use-package tsx-ts-mode
  :straight (:type built-in)
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :mode ("\\.tsx\\'" . web-mode)
  :hook (tsx-ts-mode . +javascript-add-npm-path-h)
  :hook (tsx-ts-mode . apheleia-mode)
  ;:hook (tsx-ts-mode . tsx-ts-helper-mode)
  :custom (typescript-ts-mode-indent-offset 2))
