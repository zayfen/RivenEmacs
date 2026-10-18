;;; test-init-completion-ui.el --- Tests for in-buffer completion -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression coverage for the Corfu popup-info setup: the documentation
;; popup must be loaded and globally enabled when completion is used.

;;; Code:

(require 'ert)

(defconst riven/test-init-completion-ui-repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the RivenEmacs repository.")

(let ((package-user-dir
       (expand-file-name "elpa" riven/test-init-completion-ui-repo-root)))
  (when (file-directory-p package-user-dir)
    (package-initialize)))

(condition-case nil
    (progn
      (require 'init-minibuffer)
      (require 'init-completion-ui))
  (error nil))

(ert-deftest riven/init-completion-ui-popupinfo-is-active ()
  "Corfu popup documentation is loaded, enabled, and bound to M-h."
  (skip-unless (featurep 'init-completion-ui))
  (skip-unless (featurep 'corfu-popupinfo))
  (should (bound-and-true-p corfu-popupinfo-mode))
  (should (equal corfu-popupinfo-delay '(0.25 . 0.1)))
  (should (eq (lookup-key corfu-map (kbd "M-h"))
              #'corfu-popupinfo-documentation)))

;;; test-init-completion-ui.el ends here
