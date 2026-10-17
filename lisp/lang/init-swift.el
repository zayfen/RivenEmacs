;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-swift.el --- config swift

(require 'subr-x)

;;; Locate sourcekit-lsp
(defun find-sourcekit-lsp ()
  "Return the path to sourcekit-lsp, or nil when it cannot be found.
Checks PATH first, then `xcrun -f sourcekit-lsp' on macOS, and finally the
Xcode command line tools location.  An empty `xcrun' result is treated as
\"not found\" instead of being returned as a truthy empty string."
  (or (executable-find "sourcekit-lsp")
      (let ((xcrun-path
             (and (eq system-type 'darwin)
                  (string-trim
                   (shell-command-to-string "xcrun -f sourcekit-lsp")))))
        (and (stringp xcrun-path)
             (not (string-empty-p xcrun-path))
             xcrun-path))
      (let ((toolchain-path "/usr/local/swift/usr/bin/sourcekit-lsp"))
        (and (file-executable-p toolchain-path) toolchain-path))))

(defun riven/eglot-swift-contact (&optional _interactive _project)
  "Return the Eglot contact for Swift, using sourcekit-lsp.
Optional arguments are accepted (and ignored) to satisfy eglot's
function-form contact contract (see `eglot-server-programs').  Signals a
clear error when sourcekit-lsp cannot be found."
  (let ((sourcekit (find-sourcekit-lsp)))
    (unless sourcekit
      (error "sourcekit-lsp not found (install Xcode command line tools or set PATH)"))
    (list sourcekit)))

;; Register the contact once Eglot is available.  init-eglot already adds
;; `eglot-ensure' to `swift-mode-hook'/`swift-ts-mode-hook' via
;; `rivenEmacs-lsp-modes'; the contact must be in place before Eglot guesses a
;; server for those modes.
(with-eval-after-load 'eglot
  (dolist (mode '(swift-mode swift-ts-mode))
    (setf (alist-get mode eglot-server-programs nil nil #'eq)
          #'riven/eglot-swift-contact)))

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
