;;; re-env.el --- env settings -*- lexical-binding: t; -*-

;;; Commentary:
;; env setting


(use-package direnv
  :straight t
  :hook ((prog-mode . direnv-mode)))


(provide 're-env)
