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

(ert-deftest rivenEmacs-theme-test-light-theme-is-accessible-ef-theme ()
  "The light theme defaults to ef-deuteranopia-light."
  (let* ((forms (rivenEmacs-theme-test-forms))
         (defcustom (seq-find (lambda (form)
                                (and (consp form)
                                     (eq (car form) 'defcustom)
                                     (eq (cadr form) 'rivenEmacs-light-theme)))
                              forms)))
    (should defcustom)
    ;; The standard value is a quoted symbol: (quote ef-deuteranopia-light).
    (should (eq (cadr (nth 2 defcustom)) 'ef-deuteranopia-light))
    ;; The options stay light-themed, including the default.
    (dolist (theme '(ef-deuteranopia-light ef-tritanopia-light
                     ef-cyprus ef-light))
      (should (rivenEmacs-theme-test-find
               defcustom
               (lambda (node)
                 (and (consp node)
                      (eq (car node) 'const)
                      (eq (car (last node)) theme))))))))

(ert-deftest rivenEmacs-theme-test-dark-theme-is-ef-night ()
  "The dark toggle theme defaults to ef-night."
  (let* ((forms (rivenEmacs-theme-test-forms))
         (defcustom (seq-find (lambda (form)
                                (and (consp form)
                                     (eq (car form) 'defcustom)
                                     (eq (cadr form) 'rivenEmacs-dark-theme)))
                              forms)))
    (should defcustom)
    ;; The standard value is a quoted symbol: (quote ef-night).
    (should (eq (cadr (nth 2 defcustom)) 'ef-night))
    (dolist (theme '(ef-night ef-cherie ef-atlantis ef-melissa-dark
                     ef-elea-dark ef-owl ef-dark))
      (should (rivenEmacs-theme-test-find
               defcustom
               (lambda (node)
                 (and (consp node)
                      (eq (car node) 'const)
                      (eq (car (last node)) theme))))))
    ;; <f5> toggles between the light and dark themes.
    (should (rivenEmacs-theme-test-find
             forms
             (lambda (form)
               (equal form
                      '(customize-set-variable
                        'ef-themes-to-toggle
                        (list rivenEmacs-light-theme
                              rivenEmacs-dark-theme))))))))

(ert-deftest rivenEmacs-theme-test-default-background-is-light ()
  "A light Ef theme is loaded at startup by default."
  (let* ((forms (rivenEmacs-theme-test-forms))
         (defcustom (seq-find (lambda (form)
                                (and (consp form)
                                     (eq (car form) 'defcustom)
                                     (eq (cadr form) 'rivenEmacs-default-background)))
                              forms)))
    (should defcustom)
    ;; The standard value is a quoted symbol: (quote light).
    (should (eq (cadr (nth 2 defcustom)) 'light))
    ;; Startup loads the theme family selected by `rivenEmacs-default-background'.
    (should (rivenEmacs-theme-test-find
             forms
             (lambda (form)
               (equal form
                      '(load-theme
                        (if (eq rivenEmacs-default-background 'dark)
                            rivenEmacs-dark-theme
                          rivenEmacs-light-theme)
                        t)))))))

;;; test-init-theme.el ends here
