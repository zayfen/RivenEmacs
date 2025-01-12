;;; init-lookup.el --- do lookup stuff

;;; Commentary:


;;; Code:
(use-package powerthesaurus
  :ensure t)


(use-package devdocs
  :vc (:fetcher github :repo astoff/devdocs.el)
  :commands (devdocs-lookup devdocs-install devdocs-delete devdocs-update-all)
  :init
  (add-hook 'typescript-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("typescript" "javascript" "html" "css" "node~18_lts"))))
  (add-hook 'tsx-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("react" "typescript" "javascript" "html" "css" "node~18_lts" "tailwindcss"))))
  (add-hook 'jtsx-tsx-mode-hook (lambda () (setq-local devdocs-current-docs '("react" "typescript" "javascript" "html" "css" "node~18_lts" "tailwindcss"))))
  (add-hook 'js-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("javascript" "html" "css" "node" "node~18_lts"))))
  (add-hook 'rust-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("rust"))))
  (add-hook 'python-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  (add-hook 'vue-mode-hook (lambda () (setq-local devdocs-current-docs '("vue~3" "tailwindcss" "javascript" "typescript")))))

(use-package fanyi
  :ensure t
  :commands (fanyi-dwim)
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider)))



(use-package go-translate
  :vc (:fetcher github :repo lorniu/go-translate)
  :commands (gt-do-translate)
  :config
  (setq gt-preset-translators
      `((ts-1 . ,(gt-translator
                  :taker (gt-taker :langs '(en zh) :text 'sentence)
                  :engines (gt-google-engine)
                  :render (gt-posframe-pop-render)))
        (ts-2 . ,(gt-translator
                  :taker (gt-taker :langs '(en zh) :text 'buffer
                                   :pick 'word :pick-pred (lambda (w) (length> w 6)))
                  :engines (gt-google-engine)
                  :render (gt-buffer-render))))))



(provide 'init-lookup)
;;; init-lookup.el ends here
