;;; test-init-theme.el --- Tests for init-theme -*- lexical-binding: t; -*-

(require 'ert)
(require 'seq)

(defconst rivenEmacs-theme-test-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the RivenEmacs repository.")

(defun rivenEmacs-theme-test-read-forms (file)
  "Read all top-level Lisp forms from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let (forms)
      (condition-case nil
          (while t
            (push (read (current-buffer)) forms))
        (end-of-file (nreverse forms))))))

(defun rivenEmacs-theme-test-forms ()
  "Return top-level forms from init-theme."
  (rivenEmacs-theme-test-read-forms
   (expand-file-name "lisp/ui/init-theme.el" rivenEmacs-theme-test-root)))

(ert-deftest rivenEmacs-theme-test-fancy-compilation-uses-safe-wrapper ()
  (let ((forms (rivenEmacs-theme-test-forms)))
    (should (seq-some (lambda (form)
                        (and (consp form)
                             (eq (car form) 'defun)
                             (eq (cadr form) 'rivenEmacs-enable-fancy-compilation-mode)))
                      forms))
    (should (member '(with-eval-after-load 'compile
                       (rivenEmacs-enable-fancy-compilation-mode))
                    forms))))

(defun rivenEmacs-theme-test-find (tree predicate)
  "Return the first element of TREE matching PREDICATE, or nil."
  (cond
   ((funcall predicate tree) tree)
   ((consp tree)
    (or (rivenEmacs-theme-test-find (car tree) predicate)
        (rivenEmacs-theme-test-find (cdr tree) predicate)))))

(ert-deftest rivenEmacs-theme-test-default-is-vivid-dark-ef-theme ()
  "The default theme is a dark, colorful Ef theme."
  (let* ((forms (rivenEmacs-theme-test-forms))
         (defcustom (seq-find (lambda (form)
                                (and (consp form)
                                     (eq (car form) 'defcustom)
                                     (eq (cadr form) 'rivenEmacs-default-theme)))
                              forms)))
    (should defcustom)
    ;; The standard value is a quoted symbol: (quote ef-night).
    (should (memq (cadr (nth 2 defcustom))
                  '(ef-night ef-cherie ef-atlantis ef-melissa-dark
                    ef-elea-dark ef-owl ef-dark)))
    (should (rivenEmacs-theme-test-find
             forms
             (lambda (form)
               (equal form '(load-theme rivenEmacs-default-theme t)))))))

;;; test-init-theme.el ends here
