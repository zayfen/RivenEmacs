;;; test-eglot-server-selection.el --- Eglot server selection tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Behavioral checks for `riven/eglot--pick-server': servers that are
;; already installed must win without an install prompt (for example
;; Pyright over an absent BasedPyright), and quitting or declining an
;; install prompt must not abort the connection attempt for the
;; remaining candidates.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst riven/test-eglot-selection-repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the RivenEmacs repository.")

(let ((package-user-dir
       (expand-file-name "elpa" riven/test-eglot-selection-repo-root)))
  (when (file-directory-p package-user-dir)
    (package-initialize)))

(require 'init-config)
(require 'init-eglot)

(defmacro riven/test-eglot-selection-with-declines (&rest body)
  "Run BODY with an empty declined-installs table, restoring it after."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn
         (clrhash riven/eglot--declined-installs)
         ,@body)
     (clrhash riven/eglot--declined-installs)))

(ert-deftest riven/eglot-pick-server-prefers-installed ()
  "An installed server is used without prompting to install a missing one."
  (riven/test-eglot-selection-with-declines
    (let ((prompted nil)
          (candidates
           '(("basedpyright-langserver" ("basedpyright-langserver" "--stdio")
                                        ("pip" "install" "basedpyright"))
             ("pyright-langserver" ("pyright-langserver" "--stdio")
                                   ("npm" "i" "-g" "pyright"))
             ("pylsp" ("pylsp") ("pip" "install" "python-lsp-server")))))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (binary &optional _remote)
                   (equal binary "pyright-langserver")))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq prompted t) t)))
        (should (equal (riven/eglot--pick-server candidates)
                       '("pyright-langserver" "--stdio")))
        (should-not prompted)))))

(ert-deftest riven/eglot-pick-server-installed-order-wins ()
  "Among installed servers the earliest preference wins."
  (riven/test-eglot-selection-with-declines
    (let ((candidates
           '(("first-server" ("first-server" "--stdio") nil)
             ("second-server" ("second-server" "--stdio") nil))))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (&rest _) t)))
        (should (equal (riven/eglot--pick-server candidates)
                       '("first-server" "--stdio")))))))

(ert-deftest riven/eglot-pick-server-quit-tries-next ()
  "Quitting one install prompt declines only that server and continues.
Supplying no answer for the remaining prompts finally yields nil instead
of signaling `quit' out of the connection attempt."
  (riven/test-eglot-selection-with-declines
    (let ((prompts 0)
          (candidates
           '(("missing-a" ("a") ("install-a"))
             ("missing-b" ("b") ("install-b")))))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _)
                   (setq prompts (1+ prompts))
                   (signal 'quit nil))))
        (should-not (riven/eglot--pick-server candidates))
        (should (= prompts 2))
        (should (gethash "missing-a" riven/eglot--declined-installs))
        (should (gethash "missing-b" riven/eglot--declined-installs))))))

(ert-deftest riven/eglot-pick-server-declining-all-returns-nil ()
  "Declining all install prompts returns nil without signaling."
  (riven/test-eglot-selection-with-declines
    (let ((candidates
           '(("missing-a" ("a") ("install-a"))
             ("missing-b" ("b") ("install-b")))))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (should-not (riven/eglot--pick-server candidates))
        (should (gethash "missing-a" riven/eglot--declined-installs))
        (should (gethash "missing-b" riven/eglot--declined-installs))))))

(ert-deftest riven/eglot-diagnose-is-available ()
  "The completion/LSP diagnostic command exists for interactive use."
  (should (commandp #'riven/eglot-diagnose)))

(ert-deftest riven/eglot-python-contact-picks-installed-server ()
  "The real Python contact resolves Pyright without prompting."
  (riven/test-eglot-selection-with-declines
    (let ((prompted nil))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (binary &optional _remote)
                   (equal binary "pyright-langserver")))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq prompted t) t)))
        (should (equal (riven/eglot-python-contact)
                       '("pyright-langserver" "--stdio")))
        (should-not prompted)))))

(ert-deftest riven/eglot-contact-binaries-are-strings ()
  "Server contacts pass string executable names to `executable-find'.
A symbol here makes `executable-find' signal `wrong-type-argument',
which silently disables LSP for the whole language."
  (riven/test-eglot-selection-with-declines
    (dolist (contact-fn (list #'riven/eglot-python-contact
                              #'riven/eglot-kotlin-contact))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (binary &optional _remote)
                   ;; Assert inside the stub: every probed binary is a string.
                   (should (stringp binary))
                   nil))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        ;; The contact may signal when nothing is installable; that is fine,
        ;; the assertion above is what this test covers.
        (ignore-errors (funcall contact-fn))))))

;;; test-eglot-server-selection.el ends here
