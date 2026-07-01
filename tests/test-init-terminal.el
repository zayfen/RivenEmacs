;;; test-init-terminal.el --- Tests for init-terminal -*- lexical-binding: t; -*-

(require 'ert)
(require 'keybindings-spec-core)

(defconst rivenEmacs-terminal-test-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the RivenEmacs repository.")

(defun rivenEmacs-terminal-test-read-forms (file)
  "Read all top-level Lisp forms from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let (forms)
      (condition-case nil
          (while t
            (push (read (current-buffer)) forms))
        (end-of-file (nreverse forms))))))

(defun rivenEmacs-terminal-test-use-package-names ()
  "Return package names declared by `use-package' in init-terminal."
  (let ((file (expand-file-name "lisp/tools/init-terminal.el" rivenEmacs-terminal-test-root)))
    (delq nil
          (mapcar (lambda (form)
                    (when (and (consp form) (eq (car form) 'use-package))
                      (cadr form)))
                  (rivenEmacs-terminal-test-read-forms file)))))

(defun rivenEmacs-terminal-test-use-package-form (name)
  "Return the `use-package' form for package NAME in init-terminal."
  (let ((file (expand-file-name "lisp/tools/init-terminal.el" rivenEmacs-terminal-test-root)))
    (seq-find (lambda (form)
                (and (consp form)
                     (eq (car form) 'use-package)
                     (eq (cadr form) name)))
              (rivenEmacs-terminal-test-read-forms file))))

(ert-deftest rivenEmacs-terminal-test-uses-ghostel-only ()
  (let ((packages (rivenEmacs-terminal-test-use-package-names)))
    (should (member 'ghostel packages))
    (should-not (member 'eat packages))
    (should-not (member 'vterm packages))))

(ert-deftest rivenEmacs-terminal-test-open-keybinding-uses-ghostel ()
  (should (member '("t" ghostel "Terminal") riven/keybindings-open-spec)))

(ert-deftest rivenEmacs-terminal-test-ghostel-eshell-is-local-feature ()
  (let ((form (rivenEmacs-terminal-test-use-package-form 'ghostel-eshell)))
    (should form)
    (should (plist-member (cddr form) :ensure))
    (should (eq (plist-get (cddr form) :ensure) nil))))

;;; test-init-terminal.el ends here
