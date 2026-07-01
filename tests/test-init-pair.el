;;; test-init-pair.el --- Tests for init-pair -*- lexical-binding: t; -*-

(require 'ert)
(require 'seq)

(defconst rivenEmacs-pair-test-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the RivenEmacs repository.")

(defun rivenEmacs-pair-test-read-forms (file)
  "Read all top-level Lisp forms from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let (forms)
      (condition-case nil
          (while t
            (push (read (current-buffer)) forms))
        (end-of-file (nreverse forms))))))

(defun rivenEmacs-pair-test-forms ()
  "Return top-level forms from init-pair."
  (rivenEmacs-pair-test-read-forms
   (expand-file-name "lisp/editor/init-pair.el" rivenEmacs-pair-test-root)))

(defun rivenEmacs-pair-test-use-package-form (name)
  "Return the `use-package' form for package NAME in init-pair."
  (seq-find (lambda (form)
              (and (consp form)
                   (eq (car form) 'use-package)
                   (eq (cadr form) name)))
            (rivenEmacs-pair-test-forms)))

(ert-deftest rivenEmacs-pair-test-smartparens-hook-uses-safe-wrapper ()
  (let ((forms (rivenEmacs-pair-test-forms))
        (smartparens-form (rivenEmacs-pair-test-use-package-form 'smartparens)))
    (should (seq-some (lambda (form)
                        (and (consp form)
                             (eq (car form) 'defun)
                             (eq (cadr form) 'rivenEmacs-enable-smartparens-mode)))
                      forms))
    (should smartparens-form)
    (should (equal (plist-get (cddr smartparens-form) :hook)
                   '((prog-mode text-mode markdown-mode) . rivenEmacs-enable-smartparens-mode)))))

;;; test-init-pair.el ends here
