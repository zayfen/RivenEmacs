;;; web.el --- config for web development -*- lexical-binding: t; -*-

;;; Commentary:
;; config for web development

;;; Code:
(use-package apheleia
  :straight t
  :commands apheleia-mode
  :config
  (push '(tsx-ts-mode . prettier-typescript) apheleia-mode-alist)
  (setf (alist-get 'prettier-json apheleia-formatters)
        '(npx "prettier" "--stdin-filepath" filepath)))

(defun +web/indent-or-yas-or-emmet-expand ()
  "Do-what-I-mean on TAB.

Invokes `indent-for-tab-command' if at or before text bol, `yas-expand' if on a
snippet, or `emmet-expand-yas'/`emmet-expand-line', depending on whether
`yas-minor-mode' is enabled or not."
  (interactive)
  (call-interactively
   (cond ((or (<= (current-column) (current-indentation))
              (not (eolp))
              (not (or (memq (char-after) (list ?\n ?\s ?\t))
                       (eobp))))
          #'indent-for-tab-command)
         (t
          (require 'yasnippet)
          (if (yas--templates-for-key-at-point)
              #'yas-expand
            #'emmet-expand-yas))
         (#'emmet-expand-line))))

(use-package emmet-mode
  :straight t
  :preface (defvar emmet-mode-keymap (make-sparse-keymap))
  :hook ((css-mode web-mode html-mode haml-mode nxml-mode rjsx-mode tsx-ts-mode reason-mode) . emmet-mode)
  :config
  (when (require 'yasnippet nil t)
    (add-hook 'emmet-mode-hook #'yas-minor-mode-on))
  (setq emmet-move-cursor-between-quotes t)
  (+map! :keymaps 'emmet-mode-keymap
    "E" #'emmet-wrap-with-markup
    "e" #'+web/indent-or-yas-or-emmet-expand))

(use-package web-mode
  :straight t
  :hook (web-mode . +javascript-add-npm-path-h)
  :hook (web-mode . electric-pair-mode)
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.eex\\'" . web-mode)
         ("\\.html\\.heex\\'" . web-mode)
         ("\\.html\\.tera\\'" . web-mode)
         ("\\.html\\.jinja\\'" . web-mode)
         ("\\.html\\.j2\\'" . web-mode)
         ("\\.ejs\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))

  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-indentation nil)
  (web-mode-enable-auto-pairing nil)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight nil)
  (web-mode-style-padding 0)
  (web-mode-script-padding 0)
  (web-mode-engines-alist '(("django" . "\\.html\\.tera\\'")
                            ("mustache" . "\\.vue\\")))

  :config
  (add-hook 'web-mode-hook
            #'(lambda ()
                (unless (flycheck-checker-supports-major-mode-p
                         'javascript-eslint 'web-mode)
                 (flycheck-add-mode 'javascript-eslint 'web-mode))
                (flycheck-mode t))))

(use-package css-ts-mode
  :straight (:type built-in)
  :mode (("\\.css\\'" . css-ts-mode))
  :hook (css-ts-mode . +javascript-add-npm-path-h)
  ;;  :hook (css-ts-mode . apheleia-mode)
  :custom (css-indent-offset 2))

(use-package sass-mode
  :straight t
  :custom (scss-indent-offset 2))

(use-package js-json-mode
  :straight (:type built-in)
  :hook (json-ts-mode . +javascript-add-npm-path-h)
  :hook (json-ts-mode . electric-pair-mode)
  :hook (json-ts-mode . apheleia-mode))

(use-package js-mode
  :straight (:type built-in)
  :hook (js-mode . +javascript-add-npm-path-h)
  :hook (js-mode . apheleia-mode)
  :hook (js-mode . electric-pair-mode)
  :custom (js-indent-level 2))

(use-package js-jsx-mode
  :straight (:type built-in)
  :hook (js-jsx-mode . +javascript-add-npm-path-h)
  :hook (js-jsx-mode . apheleia-mode)
  :hook (js-jsx-mode . electric-pair-mode)
  :custom (js-indent-level 2))

(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode (("\\.ts\\'" . typescript-ts-mode))
  :hook ((typescript-ts-mode . +javascript-add-npm-path-h)
         (typescript-ts-mode . apheleia-mode)
         (typescript-ts-mode . electric-pair-mode))
  :init
  (after! flycheck
    (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode))
  :custom (typescript-ts-mode-indent-offset 2)
  :config
  (set-electric! 'typescript-ts-mode :chars '(?\} ?\)) :words '("||" "&&")))


(use-package tsx-ts-mode
  :straight (:type built-in)
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :hook ((tsx-ts-mode . +javascript-add-npm-path-h)
         (tsx-ts-mode . apheleia-mode)
         (tsx-ts-mode . electric-pair-mode))
  :init
  (after! flycheck
    (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode))
  :custom (typescript-ts-mode-indent-offset 2)
  :config
  (set-electric! 'typescript-ts-mode :chars '(?\} ?\)) :words '("||" "&&")))

(use-package jsdoc
  :ensure t
  :straight (:host github :repo "isamert/jsdoc.el"))

;; for vue-mode
(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
