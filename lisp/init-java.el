;;; init-java.el --- Java and Kotlin development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for JVM languages (Java, Kotlin) development

;;; Code:

;; Kotlin Tree-sitter mode configuration
(use-package kotlin-ts-mode
  :mode ("\\.kt\\'" . kotlin-ts-mode)
  :mode ("\\.kts\\'" . kotlin-ts-mode)
  :init
  (setq kotlin-ts-mode-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.kts\\'" . kotlin-ts-mode)))

(provide 'init-java)
;;; init-java.el ends here
