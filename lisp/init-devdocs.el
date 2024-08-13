;;; init-devdocs.el --- lookup dev docs -*- lexical-binding: t; -*-





(use-package devdocs
  :vc (:fetcher github :repo astoff/devdocs.el)
  :commands (devdocs-lookup devdocs-install devdocs-delete devdocs-update-all)
  :config
  (add-hook 'typescript-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("javascript"))))
  (add-hook 'js-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("javascript"))))
  (add-hook 'rust-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("rust"))))
  (add-hook 'python-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  )


(provide 'init-devdocs)
