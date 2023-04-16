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
  :hook ((css-mode web-mode html-mode haml-mode nxml-mode rjsx-mode tsx-ts-mode reason-mode) . emmet-mode)
  :config
  (when (require 'yasnippet nil t)
    (add-hook 'emmet-mode-hook #'yas-minor-mode-on))
  (setq emmet-move-cursor-between-quotes t)
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
  (web-mode-engines-alist '(("django" . "\\.html\\.tera\\'")))

  :after smartparens
  :config


  (defun +web-is-auto-close-style-3 (_id action _context)
    (and (eq action 'insert)
         (eq web-mode-auto-close-style 3)))
  (sp-local-pair 'web-mode "<" ">" :unless '(:add +web-is-auto-close-style-3))

   ;; let smartparens handle these
  (setq web-mode-enable-auto-quoting nil
        web-mode-enable-auto-pairing t)

   ;; 1. Remove web-mode auto pairs whose end pair starts with a latter
   ;;    (truncated autopairs like <?p and hp ?>). Smartparens handles these
   ;;    better.
   ;; 2. Strips out extra closing pairs to prevent redundant characters
   ;;    inserted by smartparens.
  (dolist (alist web-mode-engines-auto-pairs)
    (setcdr alist
            (cl-loop for pair in (cdr alist)
                     unless (string-match-p "^[a-z-]" (cdr pair))
                     collect (cons (car pair)
                                   (string-trim-right (cdr pair)
                                                      "\\(?:>\\|]\\|}\\)+\\'")))))
  (delq! nil web-mode-engines-auto-pairs))

;; (add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.ts[x]?\\'")))

(use-package css-ts-mode
  :straight (:type built-in)
  :mode (("\\.css\\'" . css-ts-mode))
  :hook (css-ts-mode . +javascript-add-npm-path-h)
  :hook (css-ts-mode . apheleia-mode)
  :custom (css-indent-offset 2))

(use-package sass-mode
  :straight t
  :custom (scss-indent-offset 2))

(use-package json-ts-mode
  :straight (:type built-in)
  :mode (("\\.json\\'" . json-ts-mode))
  :hook (json-ts-mode . smartparens-mode)
  :hook (json-ts-mode . +javascript-add-npm-path-h)
  :hook (json-ts-mode . electric-pair-mode)
  :hook (json-ts-mode . apheleia-mode))

(use-package js-ts-mode
  :straight (:type built-in)
  :mode ("\\.js\\'" . js-ts-mode)
  :hook (js-ts-mode . +javascript-add-npm-path-h)
  :hook (js-ts-mode . smartparens-mode)
  :hook (js-ts-mode . apheleia-mode)
  :hook (js-ts-mode . electric-pair-mode)  tsx
  :custom (js-indent-level 2))

(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode (("\\.ts\\'" . typescript-ts-mode))
  :hook (typescript-ts-mode . +javascript-add-npm-path-h)
  :hook (typescript-ts-mode . apheleia-mode)
  :hook (typescript-ts-mode . smartparens-mode)
  :hook (typescript-ts-mode . electric-pair-mode)
  :custom (typescript-ts-mode-indent-offset 2)
  :config
  (set-electric! 'typescript-ts-mode :chars '(?\} ?\)) :words '("||" "&&"))
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
  (flycheck-add-mode 'typescript-tslint 'typescript-ts-mode))


(use-package tsx-ts-mode
  :straight (:type built-in)
  :hook (tsx-ts-mode . +javascript-add-npm-path-h)
  :hook (tsx-ts-mode . apheleia-mode)
  :hook (tsx-ts-mode . smartparens-mode)
  :hook (tsx-ts-mode . electric-pair-mode)
;;  :hook (tsx-ts-mode . web-mode)
  :custom (typescript-ts-mode-indent-offset 2)
  :config
  (set-electric! 'typescript-ts-mode :chars '(?\} ?\)) :words '("||" "&&"))
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)
  (flycheck-add-mode 'typescript-tslint 'tsx-ts-mode))



(setq auto-mode-alist
   (append '(("\\.tsx\\'" . tsx-ts-mode)  ; note these are encapsulated in a '() list
             ("\\.ts\\'" . typescript-ts-mode))
           auto-mode-alist))
