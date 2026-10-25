;;; test-eglot-signature-help.el --- Signature help cycling tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Behavioral coverage for `riven/eglot-signature-next' and
;; `riven/eglot-signature-previous': cycling through LSP signature-help
;; overloads, wrapping around, resetting the cycle when point moves, and
;; failing clearly without a server or signature-help support.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst riven/test-eglot-signature-help-repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the RivenEmacs repository.")

(let ((package-user-dir
       (expand-file-name "elpa" riven/test-eglot-signature-help-repo-root)))
  (when (file-directory-p package-user-dir)
    (package-initialize)))

(require 'init-config)
(require 'init-eglot)
(require 'eglot)

(defun riven/test-signature-help-response (&optional labels)
  "Return a fake signature-help response for LABELS.
LABELS defaults to three overloads; the first one is active."
  (let ((labels (or labels '("f(a)" "f(a, b)" "f()"))))
    (list :signatures
          (vconcat (mapcar (lambda (label)
                             (list :label label
                                   :documentation nil
                                   :parameters (vector (list :label "a"))))
                           labels))
          :activeSignature 0
          :activeParameter 0)))

(defmacro riven/test-signature-help-environment (&rest body)
  "Run BODY in a managed-looking buffer with stubbed Eglot requests.
The variable MESSAGES collects everything passed to `message'."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert "(f ")
     (setq-local eglot--managed-mode t)
     (let ((riven/eglot-signature--state nil)
           (messages nil))
       (cl-letf (((symbol-function 'eglot-current-server)
                  (lambda () 'fake-server))
                 ((symbol-function 'eglot-server-capable)
                  (lambda (&rest _) t))
                 ((symbol-function 'eglot--TextDocumentIdentifier)
                  (lambda () '(:uri "file:///tmp/app.py")))
                 ((symbol-function 'eglot--pos-to-lsp-position)
                  (lambda (&optional _pos) '(:line 0 :character 3)))
                 ((symbol-function 'eglot--request)
                  (lambda (&rest _) (riven/test-signature-help-response)))
                 ((symbol-function 'message)
                  (lambda (format-string &rest args)
                    (push (if (stringp format-string)
                              (apply #'format format-string args)
                            format-string)
                          messages))))
         ,@body))))

(ert-deftest riven/eglot-signature-next-cycles-and-wraps ()
  "Next moves through overloads and wraps back to the first."
  (riven/test-signature-help-environment
    (riven/eglot-signature-next)
    (should (string-match-p "\\`\\[2/3\\] f(a, b)" (car messages)))
    (riven/eglot-signature-next)
    (should (string-match-p "\\`\\[3/3\\] f()" (car messages)))
    (riven/eglot-signature-next)
    (should (string-match-p "\\`\\[1/3\\] f(a)" (car messages)))))

(ert-deftest riven/eglot-signature-previous-cycles-backward ()
  "Previous moves backwards, wrapping from the first overload to the last."
  (riven/test-signature-help-environment
    (riven/eglot-signature-previous)
    (should (string-match-p "\\`\\[3/3\\]" (car messages)))
    (riven/eglot-signature-previous)
    (should (string-match-p "\\`\\[2/3\\]" (car messages)))))

(ert-deftest riven/eglot-signature-cycle-resets-when-point-moves ()
  "Moving point starts a fresh cycle based on the server-active signature."
  (riven/test-signature-help-environment
    (riven/eglot-signature-next)
    (should (string-match-p "\\`\\[2/3\\]" (car messages)))
    (goto-char (point-min))
    (riven/eglot-signature-next)
    (should (string-match-p "\\`\\[2/3\\]" (car messages)))))

(ert-deftest riven/eglot-signature-cycle-handles-single-overload ()
  "A single overload keeps reporting [1/1]."
  (riven/test-signature-help-environment
    (cl-letf (((symbol-function 'eglot--request)
               (lambda (&rest _)
                 (riven/test-signature-help-response '("only(a)")))))
      (riven/eglot-signature-next)
      (should (string-match-p "\\`\\[1/1\\] only(a)" (car messages)))
      (riven/eglot-signature-next)
      (should (string-match-p "\\`\\[1/1\\]" (car messages))))))

(ert-deftest riven/eglot-signature-cycle-requires-server ()
  "Cycling without a live server signals a clear user error."
  (riven/test-signature-help-environment
    (cl-letf (((symbol-function 'eglot-current-server) (lambda () nil)))
      (should-error (riven/eglot-signature-next) :type 'user-error))))

(ert-deftest riven/eglot-signature-cycle-requires-capability ()
  "Cycling without signature-help support signals a clear user error."
  (riven/test-signature-help-environment
    (cl-letf (((symbol-function 'eglot-server-capable)
               (lambda (&rest _) nil)))
      (should-error (riven/eglot-signature-next) :type 'user-error))))

(provide 'test-eglot-signature-help)
;;; test-eglot-signature-help.el ends here
