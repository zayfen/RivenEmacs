



;; install expand-region

(use-package expand-region
  :vc (:fetcher github :repo magnars/expand-region.el)
  :bind ("C-=" . er/expand-region))

(use-package iedit
  :vc (:fetcher github :repo victorhge/iedit))

(use-package avy
  :vc (:fetcher github :repo abo-abo/avy)
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-c C-j") 'avy-resume))
  

(provide 'init-editor)
