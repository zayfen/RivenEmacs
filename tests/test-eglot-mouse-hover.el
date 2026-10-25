;;; test-eglot-mouse-hover.el --- Mouse hover documentation tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Behavioral coverage for `riven/eglot-mouse-hover': formatting of the
;; echo-area text, resolution of the symbol under a mouse position, and the
;; `eglot-mode-map' binding installed by `riven/eglot-mouse-hover-mode'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst riven/test-eglot-mouse-hover-repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the RivenEmacs repository.")

(let ((package-user-dir
       (expand-file-name "elpa" riven/test-eglot-mouse-hover-repo-root)))
  (when (file-directory-p package-user-dir)
    (package-initialize)))

(require 'init-config)
(require 'init-eglot)
;; Loading Eglot runs the deferred `use-package eglot' :config, which installs
;; the mouse-movement binding when `rivenEmacs-eglot-mouse-hover' is non-nil.
(require 'eglot)

(ert-deftest riven/eglot-mouse-hover-format-trims-and-truncates ()
  "Hover text is trimmed and limited to the configured line count."
  (should (equal (riven/eglot-mouse-hover--format "\nline1\nline2\n\n")
                 "line1\nline2"))
  (let ((rivenEmacs-eglot-mouse-hover-max-lines 2))
    (let ((formatted (riven/eglot-mouse-hover--format "one\ntwo\nthree\nfour")))
      (should (string-prefix-p "one\ntwo" formatted))
      (should (string-match-p "2 more line" formatted)))))

(ert-deftest riven/eglot-mouse-hover-format-drops-code-fences ()
  "Markdown code fences are removed from the echo-area text."
  (should (equal (riven/eglot-mouse-hover--format
                  "```python\ndef greet(name):\n    ...\n```")
                 "def greet(name):\n    ...")))

(ert-deftest riven/eglot-mouse-hover-format-keeps-indentation ()
  "Inner indentation is preserved while trailing whitespace is dropped."
  (should (equal (riven/eglot-mouse-hover--format "sig\n    doc  ")
                 "sig\n    doc")))

(ert-deftest riven/eglot-mouse-hover-symbol-at-managed-buffer ()
  "A mouse position over a symbol in a managed buffer resolves the symbol."
  (with-temp-buffer
    (insert "foo bar")
    (set-window-buffer (selected-window) (current-buffer))
    (setq-local eglot--managed-mode t)
    (cl-letf (((symbol-function 'eglot-current-server)
               (lambda () 'fake-server)))
      (should (equal (riven/eglot-mouse-hover--symbol-at
                      (list (selected-window) 2 '(1 . 1) 0))
                     (list (current-buffer) 1 "foo"))))))

(ert-deftest riven/eglot-mouse-hover-symbol-at-blank-area ()
  "A mouse position over blank text yields no hover target."
  (with-temp-buffer
    (insert "   ")
    (set-window-buffer (selected-window) (current-buffer))
    (setq-local eglot--managed-mode t)
    (cl-letf (((symbol-function 'eglot-current-server)
               (lambda () 'fake-server)))
      (should-not (riven/eglot-mouse-hover--symbol-at
                   (list (selected-window) 2 '(1 . 1) 0))))))

(ert-deftest riven/eglot-mouse-hover-symbol-at-unmanaged-buffer ()
  "A mouse position in an unmanaged buffer yields no hover target."
  (with-temp-buffer
    (insert "foo")
    (set-window-buffer (selected-window) (current-buffer))
    (setq-local eglot--managed-mode nil)
    (cl-letf (((symbol-function 'eglot-current-server) (lambda () nil)))
      (should-not (riven/eglot-mouse-hover--symbol-at
                   (list (selected-window) 1 '(1 . 1) 0))))))

(ert-deftest riven/eglot-mouse-hover-displays-and-hides-documentation ()
  "The fetch callback shows hover text and `--hide' clears it."
  (with-temp-buffer
    (insert "foo")
    (set-window-buffer (selected-window) (current-buffer))
    (setq-local eglot--managed-mode t)
    (let ((riven/eglot-mouse-hover--last nil)
          (riven/eglot-mouse-hover--shown nil)
          (messages nil))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda () 'fake-server))
                ((symbol-function 'eglot-hover-eldoc-function)
                 (lambda (cb &rest _ignored)
                   (funcall cb "docs for foo" :echo 0)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (if (stringp format-string)
                             (apply #'format format-string args)
                           format-string)
                         messages))))
        (riven/eglot-mouse-hover--fetch (list (current-buffer) 1 "foo"))
        (should (equal (car messages) "docs for foo"))
        (should riven/eglot-mouse-hover--shown)
        (riven/eglot-mouse-hover--hide)
        (should (equal (car messages) nil))
        (should-not riven/eglot-mouse-hover--shown)))))

(ert-deftest riven/eglot-mouse-hover-schedules-idle-request ()
  "The mouse-movement command schedules a debounced idle request."
  (with-temp-buffer
    (insert "foo")
    (set-window-buffer (selected-window) (current-buffer))
    (setq-local eglot--managed-mode t)
    (let ((riven/eglot-mouse-hover--last nil))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda () 'fake-server)))
        (unwind-protect
            (progn
              (riven/eglot-mouse-hover
               (list 'mouse-movement
                     (list (selected-window) 1 '(1 . 1) 0)))
              (should (timerp riven/eglot-mouse-hover--timer)))
          (riven/eglot-mouse-hover--cancel))))))

(ert-deftest riven/eglot-mouse-hover-mode-binds-mouse-movement ()
  "The hover mode binds and unbinds `mouse-movement' in `eglot-mode-map'."
  (should (commandp #'riven/eglot-mouse-hover))
  (unwind-protect
      (progn
        (riven/eglot-mouse-hover-mode 1)
        (should (eq (lookup-key eglot-mode-map [mouse-movement])
                    #'riven/eglot-mouse-hover))
        (riven/eglot-mouse-hover-mode -1)
        (should-not (eq (lookup-key eglot-mode-map [mouse-movement])
                        #'riven/eglot-mouse-hover)))
    (when rivenEmacs-eglot-mouse-hover
      (riven/eglot-mouse-hover-mode 1))))

;;; test-eglot-mouse-hover.el ends here
