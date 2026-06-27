;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-agent.el --- Bridge to the Python website agent -*- lexical-binding: t; -*-

;;; Commentary:
;; Synchronous bridge from Emacs to `codeforces_agent.py'.  The Python agent
;; (curl-cffi) bypasses Cloudflare for website operations that Emacs cannot do
;; directly: fetching problem statements and submitting solutions.
;;
;; Calling convention (mirrors RivenEmacs init-gpt.el / init-gpt-helper.el):
;;   `call-process' with a temp buffer for stdout and a separate buffer for
;;   stderr; non-zero exit signals an error carrying the stderr text.

;;; Code:

(defconst +cf--module-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory containing this module's source file (captured at load time).")

(defun +cf-agent--script ()
  "Return the absolute path to the Python agent script."
  (expand-file-name "scripts/codeforces_agent.py" +cf--module-dir))

(defun +cf-agent--python ()
  "Return the python executable, or error."
  (or (executable-find "python3")
      (executable-find "python")
      (error "python3 not found on PATH (needed by the Codeforces agent)")))

(defun +cf-agent-call (args)
  "Run the Codeforces agent with ARGS (a list of strings).
Return its stdout string.  On non-zero exit, signal an error including stderr."
  (let ((script (+cf-agent--script))
        (exe (+cf-agent--python))
        (stderr-file (make-temp-file "cf-agent-err")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'call-process
                               exe nil (list t stderr-file) nil
                               script args)))
            (cond
             ((and (integerp status) (zerop status))
              (string-trim-right (buffer-string)))
             ((stringp status)
              (error "Codeforces agent killed: %s" status))
             (t
              (let ((err (with-temp-buffer
                           (insert-file-contents stderr-file)
                           (string-trim (buffer-string)))))
                (error "Codeforces agent failed (status %s): %s" status err))))))
      (delete-file stderr-file))))

(defun +cf-agent-fetch-statement (problem)
  "Fetch PROBLEM's statement and return it as Org text.
PROBLEM is a plist with :contestId and :index."
  (+cf-agent-call
   (list "statement"
         "--contest" (number-to-string (plist-get problem :contestId))
         "--index" (plist-get problem :index))))

(defun +cf-agent-submit (problem language source-file)
  "Submit the solution at SOURCE-FILE for PROBLEM as LANGUAGE.
Returns the new submission id (string)."
  (+cf-agent-call
   (list "submit"
         "--contest" (number-to-string (plist-get problem :contestId))
         "--index" (plist-get problem :index)
         "--lang" language
         "--source-file" source-file)))

(defun +cf-agent-login-check ()
  "Verify the stored cookie is valid.  Return handle on success, nil on failure."
  (condition-case nil
      (let ((out (+cf-agent-call (list "login-check"))))
        (and (not (string-empty-p out)) out))
    (error nil)))

(provide 'emacs-codeforces-agent)

;;; emacs-codeforces-agent.el ends here
