;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-swift.el --- config swift

;;; Locate sourcekit-lsp
(defun find-sourcekit-lsp ()
  (or (executable-find "sourcekit-lsp")
      (and (eq system-type 'darwin)
           (string-trim (shell-command-to-string "xcrun -f sourcekit-lsp")))
      "/usr/local/swift/usr/bin/sourcekit-lsp"))


;; Swift editing support
(use-package swift-mode
    :ensure t
    :mode "\\.swift\\'"
    :interpreter "swift")

;; (use-package lsp-sourcekit
;;     :ensure t
;;     :after lsp-mode
;;     :custom
;;     (lsp-sourcekit-executable (find-sourcekit-lsp) "Find sourcekit-lsp"))

(provide 'init-swift)
;;; init-swift.el ends here
