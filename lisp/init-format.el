;;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-format.el --- Config for code format -*- lexical-binding: t; -*-

(use-package aggressive-indent-mode
  :vc (:fetcher github :repo Malabarba/aggressive-indent-mode)
  :hook ((json-ts-mode . aggressive-indent-mode)
         (css-ts-mode . aggressive-indent-mode)
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'tsx-ts-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'typescript-ts-mode)

  ;; The variable aggressive-indent-dont-indent-if lets you customize when you don't want indentation to happen.
  ;; For instance, if you think it's annoying that lines jump around in c++-mode because you haven't typed the ;
  ;; yet, you could add the following clause:
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode 'c-mode 'csharp-mode 'c++-ts-mode 'c-ts-mode 'csharp-ts-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))


(use-package format-all
  :vc (:fetcher github :repo lassik/emacs-format-all-the-code)
  :commands format-all-mode
  ;; :hook (prog-mode . format-all-mode) ;; dont want format code on save
  :bind ("M-I" . format-all-region-or-buffer)
  :config
  (setq-default format-all-formatters
                '(("C"     (astyle "--mode=c"))
                  ("Shell" (shfmt "-i" "4" "-ci")))))


;; 开启 ESLint 的自动修复模式：需要预先在全局安装 eslint_d 包
;; (use-package reformatter
;;   :config
;;   (progn
;;     (reformatter-define eslintd-fix
;;       :program (executable-find "eslint_d")
;;       :args (list "--fix-to-stdout" "--stdin" "--stdin-filename" (buffer-file-name))
;;       :input-file (reformatter-temp-file-in-current-directory "js")
;;       :exit-code-success-p (lambda (code) (or (eq code 1) (eq code 0))))))



(provide 'init-format)
