;;; re-indent.el --- Config for code indent -*- lexical-binding: t; -*-

(use-package aggressive-indent-mode
  :straight t
  :ensure t
  :hook ((typescript-ts-mode . aggressive-indent-mode)
         (tsx-ts-mode . aggressive-indent-mode)
         (json-ts-mode . aggressive-indent-mode)
         (css-ts-mode . aggressive-indent-mode)
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)

  ;; The variable aggressive-indent-dont-indent-if lets you customize when you don't want indentation to happen.
  ;; For instance, if you think it's annoying that lines jump around in c++-mode because you haven't typed the ;
  ;; yet, you could add the following clause:
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode 'c-mode 'csharp-mode 'c++-ts-mode 'c-ts-mode 'csharp-ts-mode)
     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
            (thing-at-point 'line))))))

(provide 're-indent)
