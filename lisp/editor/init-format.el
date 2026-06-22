;;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-format.el --- Config for code format -*- lexical-binding: t; -*-

(defcustom riven/aggressive-indent-max-size (* 3000 80)
  "Buffers larger than this many bytes skip `aggressive-indent-mode'.
Aggressive indent re-indents on every edit, which is prohibitively expensive
in large buffers."
  :type 'integer
  :group 'rivenEmacs)

(defun riven/maybe-enable-aggressive-indent ()
  "Enable `aggressive-indent-mode' unless the buffer is too large."
  (when (< (buffer-size) riven/aggressive-indent-max-size)
    (aggressive-indent-mode 1)))

(use-package aggressive-indent-mode
  :vc (:url "https://github.com/Malabarba/aggressive-indent-mode")
  ;; Enable inside the specific mode hooks only after the size check, instead
  ;; of running on every `find-file' and disabling after the fact.
  :hook ((json-ts-mode . riven/maybe-enable-aggressive-indent)
         (css-ts-mode  . riven/maybe-enable-aggressive-indent))
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


(defun riven/apheleia-format-dwim ()
  "Format current region if active, otherwise format the whole buffer."
  (interactive)
  (if (and (use-region-p) (fboundp 'apheleia-format-region))
      (apheleia-format-region (region-beginning) (region-end))
    (apheleia-format-buffer)))

(use-package apheleia
  :ensure t
  :commands (apheleia-format-buffer apheleia-format-region)
  :bind ("C-S-i" . riven/apheleia-format-dwim)
  :config
  ;; Keep shell formatting options consistent with previous setup.
  (setf (alist-get 'riven/shfmt apheleia-formatters)
        '("shfmt" "-i" "4" "-ci"))
  (setf (alist-get 'sh-mode apheleia-mode-alist) 'riven/shfmt)
  (setf (alist-get 'bash-ts-mode apheleia-mode-alist) 'riven/shfmt))


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
