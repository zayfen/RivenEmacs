;;; init-lookup.el --- do lookup stuff

;;; Commentary:


;;; Code:
(use-package powerthesaurus
  :ensure t)


(use-package devdocs
  :vc (:fetcher github :repo astoff/devdocs.el)
  :commands (devdocs-lookup devdocs-install devdocs-delete devdocs-update-all)
  :config
  (add-hook 'typescript-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("javascript"))))
  (add-hook 'js-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("javascript"))))
  (add-hook 'rust-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("rust"))))
  (add-hook 'python-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  )

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
  :vc (:fetcher github :repo )
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
