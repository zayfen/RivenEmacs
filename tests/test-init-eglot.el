;;; test-init-eglot.el --- Tests for Eglot configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Form-level checks for the LSP wiring: workspace defaults, disabled
;; on-type formatting, and the HTML/JSON/YAML/JSX coverage added on top of
;; Eglot's built-in contacts.

;;; Code:

(require 'ert)
(require 'seq)
(require 'init-config)

(defconst riven/test-init-eglot-repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the RivenEmacs repository.")

(defun riven/test-init-eglot-read-forms (file)
  "Read all top-level forms from FILE."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file riven/test-init-eglot-repo-root))
    (let (forms)
      (condition-case nil
          (while t
            (push (read (current-buffer)) forms))
        (end-of-file (nreverse forms))))))

(defun riven/test-init-eglot-find (tree predicate)
  "Return the first element of TREE matching PREDICATE, or nil."
  (cond
   ((funcall predicate tree) tree)
   ((consp tree)
    (or (riven/test-init-eglot-find (car tree) predicate)
        (riven/test-init-eglot-find (cdr tree) predicate)))))

(defun riven/test-init-eglot-find-call (forms call predicate)
  "Find a nested CALL form in FORMS whose arguments satisfy PREDICATE."
  (riven/test-init-eglot-find
   forms
   (lambda (node)
     (and (consp node)
          (eq (car node) call)
          (funcall predicate node)))))

(ert-deftest riven/eglot-lsp-mode-coverage ()
  "Web/JSON/YAML modes are included in the LSP auto-enable list."
  (dolist (mode '(web-mode js-json-mode json-ts-mode yaml-mode
                  yaml-ts-mode yaml-ts-mode-maybe
                  go-mode go-ts-mode lua-mode lua-ts-mode
                  sh-mode bash-ts-mode))
    (should (memq mode rivenEmacs-lsp-modes))))

(ert-deftest riven/eglot-workspace-configuration-defaults ()
  "The workspace configuration defcustom and function exist."
  (let ((forms (riven/test-init-eglot-read-forms "lisp/ide/init-eglot.el")))
    (should (riven/test-init-eglot-find
             forms
             (lambda (node)
               (and (consp node)
                    (eq (car node) 'defcustom)
                    (eq (cadr node) 'rivenEmacs-eglot-workspace-configuration)))))
    (should (riven/test-init-eglot-find
             forms
             (lambda (node)
               (and (consp node)
                    (eq (car node) 'defun)
                    (eq (cadr node) 'riven/eglot-workspace-configuration)))))))

(ert-deftest riven/eglot-disables-on-type-formatting ()
  "`documentOnTypeFormattingProvider' is actually ignored."
  (let ((forms (riven/test-init-eglot-read-forms "lisp/ide/init-eglot.el")))
    (should (riven/test-init-eglot-find-call
             forms 'setq
             (lambda (node)
               (and (equal (nth 1 node) 'eglot-ignored-server-capabilities)
                    (equal (cadr (nth 2 node))
                           '(:documentOnTypeFormattingProvider))))))))

(ert-deftest riven/eglot-web-json-yaml-contacts ()
  "Web, JSON and YAML contacts are registered with install commands."
  (let ((forms (riven/test-init-eglot-read-forms "lisp/ide/init-eglot.el")))
    (dolist (entry '((web-mode "vscode-html-language-server")
                     (js-json-mode "vscode-json-language-server")
                     (yaml-mode "yaml-language-server")
                     (go-mode "gopls")
                     (lua-mode "lua-language-server")
                     (sh-mode "bash-language-server")))
      (should (riven/test-init-eglot-find-call
               forms 'riven/eglot-register-simple-server
               (lambda (node)
                 (let ((modes (nth 1 node)))
                   (and (consp modes)
                        (eq (car modes) 'quote)
                        (memq (car entry) (cadr modes))
                        (equal (nth 2 node) (cadr entry))))))))))

(ert-deftest riven/eglot-install-robustness ()
  "Install timeout, file-watch, inlay-hint and reset controls exist."
  (let ((forms (riven/test-init-eglot-read-forms "lisp/ide/init-eglot.el")))
    (dolist (name '(rivenEmacs-eglot-install-timeout
                    rivenEmacs-eglot-disable-file-watch
                    rivenEmacs-eglot-inlay-hint-modes))
      (should (riven/test-init-eglot-find
               forms
               (lambda (node)
                 (and (consp node)
                      (eq (car node) 'defcustom)
                      (eq (cadr node) name))))))
    (dolist (name '(riven/eglot-reset-install-prompts
                    riven/eglot--run-install
                    riven/eglot--refresh-exec-path
                    riven/eglot--maybe-enable-inlay-hints))
      (should (riven/test-init-eglot-find
               forms
               (lambda (node)
                 (and (consp node)
                      (eq (car node) 'defun)
                      (eq (cadr node) name))))))))

(ert-deftest riven/eglot-preserves-mode-language-ids ()
  "Overridden contacts keep the language IDs expected by their servers."
  (let ((forms (riven/test-init-eglot-read-forms "lisp/ide/init-eglot.el")))
    (dolist (entry '((js-ts-mode . "javascript")
                     (typescript-ts-mode . "typescript")
                     (tsx-ts-mode . "typescriptreact")
                     (web-mode . "html")
                     (js-json-mode . "json")
                     (yaml-mode . "yaml")
                     (rust-ts-mode . "rust")
                     (c-ts-mode . "c")
                     (c++-ts-mode . "c++")
                     (java-ts-mode . "java")
                     (php-ts-mode . "php")
                     (go-ts-mode . "go")
                     (lua-ts-mode . "lua")
                     (bash-ts-mode . "shellscript")))
      (should (riven/test-init-eglot-find-call
               forms 'riven/eglot-register-simple-server
               (lambda (node)
                 (let ((modes (nth 1 node)))
                   (and (consp modes)
                        (eq (car modes) 'quote)
                        (memq (car entry) (cadr modes))
                        (equal (nth 5 node) (cdr entry)))))))))
  (let ((forms (riven/test-init-eglot-read-forms "lisp/ide/init-eglot.el")))
    (dolist (key '((python-ts-mode :language-id "python")
                   (vue-ts-mode :language-id "vue")
                   (kotlin-ts-mode :language-id "kotlin")))
      (should (riven/test-init-eglot-find forms
                                          (lambda (node) (equal node key)))))))

(ert-deftest riven/eglot-jsx-uses-tree-sitter-mode ()
  "`auto-mode-alist' maps .jsx to `js-ts-mode'."
  (let ((forms (riven/test-init-eglot-read-forms "lisp/lang/init-web.el")))
    (should (riven/test-init-eglot-find
             forms
             (lambda (node)
               (equal node
                      '(add-to-list 'auto-mode-alist
                                    '("\\.jsx$" . js-ts-mode))))))))

;;; test-init-eglot.el ends here
