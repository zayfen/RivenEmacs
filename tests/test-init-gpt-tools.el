;;; test-init-gpt-tools.el --- Tests for gptel core tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline checks for the Work preset and the confined core tools.  No
;; network or shell tool is exercised here; those are opt-in and confirmed.

;;; Code:

(require 'ert)
(require 'package)
(require 'seq)

(defconst riven/test-gpt-tools-repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the RivenEmacs repository.")

(let ((package-user-dir
       (expand-file-name "elpa" riven/test-gpt-tools-repo-root)))
  (when (file-directory-p package-user-dir)
    (package-initialize)))

(when (require 'gptel nil t)
  (require 'init-gpt-tools nil t))

(defconst riven/test-gpt-tools-names
  '("read_file" "list_directory" "grep_files" "write_file"
    "run_shell_command" "web_search" "read_url")
  "Core tools registered by init-gpt-tools.")

(ert-deftest riven/gptel-core-tools-are-registered ()
  "All core gptel tools are in gptel's tool registry."
  (skip-unless (featurep 'init-gpt-tools))
  (dolist (name riven/test-gpt-tools-names)
    (should (gptel-get-tool name))))

(ert-deftest riven/gptel-work-preset-uses-core-tools ()
  "The Work preset activates the core tools and confirmation."
  (skip-unless (featurep 'init-gpt-tools))
  (let ((preset (gptel-get-preset 'work)))
    (should preset)
    (should (equal (plist-get preset :tools)
                   riven/test-gpt-tools-names))
    (should (eq (plist-get preset :use-tools) t))
    (should (eq (plist-get preset :confirm-tool-calls) 'auto))
    (should (eq (plist-get preset :include-tool-results) 'call))))

(ert-deftest riven/gptel-tools-read-write-confined-root ()
  "read_file/write_file stay inside `rivenEmacs-gptel-tool-roots'."
  (skip-unless (featurep 'init-gpt-tools))
  (let* ((dir (make-temp-file "riven-gptel-test-" t))
         (rivenEmacs-gptel-tool-roots (list dir)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.txt" dir)
            (insert "hello"))
          (should (equal (riven/gptel-tool--read-file "a.txt") "hello"))
          (riven/gptel-tool--write-file "sub/b.txt" "world")
          (should (equal (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "sub/b.txt" dir))
                           (buffer-string))
                         "world"))
          (should-error (riven/gptel-tool--path "../etc/passwd"))
          (should-error (riven/gptel-tool--read-file "../etc/passwd")))
      (delete-directory dir t))))

;;; test-init-gpt-tools.el ends here
