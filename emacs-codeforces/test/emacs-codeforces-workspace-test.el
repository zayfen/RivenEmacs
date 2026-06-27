;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'ert)
(require 'emacs-codeforces-workspace)

(ert-deftest +cf--language-extension/map ()
  "Language name maps to the expected file extension."
  (should (equal (+cf--language-extension "rust") "rs"))
  (should (equal (+cf--language-extension "python") "py"))
  (should (equal (+cf--language-extension "cpp") "cpp"))
  (should (equal (+cf--language-extension "java") "java")))

(ert-deftest +cf--problem-dir/format ()
  "Problem dir is {contestId}_{index} under workspace/."
  (let ((codeforces-home-directory "/tmp/cf/"))
    (should (equal (+cf--problem-dir '(:contestId 1234 :index "A"))
                   "/tmp/cf/workspace/1234_A"))
    ;; multi-char index like F2
    (should (equal (+cf--problem-dir '(:contestId 1234 :index "F2"))
                   "/tmp/cf/workspace/1234_F2"))))

(ert-deftest +cf-init-solution/creates-template-and-dir ()
  "Init creates the problem dir and writes the template; doesn't overwrite."
  (let ((codeforces-home-directory
         (expand-file-name (make-temp-name "cf-ws-test-") temporary-file-directory))
        (codeforces-templates-directory nil))
    (unwind-protect
        (let* ((problem '(:contestId 5 :index "B"))
               (sol (+cf-init-solution problem "rust")))
          ;; file exists at the expected path
          (should (file-exists-p sol))
          (should (string-match-p "/workspace/5_B/solution.rs$" sol))
          ;; template content present
          (with-temp-buffer
            (insert-file-contents sol)
            (should (string-match-p "fn main" (buffer-string))))
          ;; idempotent: does not overwrite an edited solution
          (with-temp-buffer
            (insert "MY EDIT")
            (write-region (point-min) (point-max) sol nil 'silent))
          (+cf-init-solution problem "rust")
          (with-temp-buffer
            (insert-file-contents sol)
            (should (string-match-p "MY EDIT" (buffer-string)))))
      (delete-directory codeforces-home-directory t))))
