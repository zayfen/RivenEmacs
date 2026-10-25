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

(defun +cf-ws-test--with-home (body)
  "Call BODY with a fresh temporary `codeforces-home-directory'."
  (let ((codeforces-home-directory
         (expand-file-name (make-temp-name "cf-ws-test-") temporary-file-directory))
        (codeforces-templates-directory nil))
    (unwind-protect
        (funcall body)
      (delete-directory codeforces-home-directory t))))

(ert-deftest +cf-init-solution/creates-cargo-project-for-rust ()
  "Rust workspaces get a Cargo.toml so rust-analyzer can analyze them."
  (+cf-ws-test--with-home
   (lambda ()
     (let* ((problem '(:contestId 1001 :index "H"))
            (sol (+cf-init-solution problem "rust"))
            (manifest (expand-file-name "Cargo.toml" (file-name-directory sol))))
       (should (file-exists-p manifest))
       (with-temp-buffer
         (insert-file-contents manifest)
         (let ((text (buffer-string)))
           (should (string-match-p "^\\[package\\]" text))
           (should (string-match-p "name = \"cf-1001-h\"" text))
           (should (string-match-p "\\[\\[bin\\]\\]" text))
           (should (string-match-p "path = \"solution.rs\"" text))))))))

(ert-deftest +cf-init-solution/no-cargo-project-for-python ()
  "Python workspaces do not get a Cargo.toml."
  (+cf-ws-test--with-home
   (lambda ()
     (let* ((problem '(:contestId 1002 :index "D2"))
            (sol (+cf-init-solution problem "python")))
       (should (file-exists-p sol))
       (should-not (file-exists-p
                    (expand-file-name "Cargo.toml" (file-name-directory sol))))))))

(ert-deftest +cf-init-solution/repairs-blank-cargo-manifest ()
  "An empty hand-made Cargo.toml is replaced with a usable manifest."
  (+cf-ws-test--with-home
   (lambda ()
     (let* ((problem '(:contestId 7 :index "A"))
            (sol (+cf-init-solution problem "rust"))
            (manifest (expand-file-name "Cargo.toml" (file-name-directory sol))))
       ;; Simulate the 0-byte placeholder a user may have created.
       (with-temp-file manifest nil)
       (+cf-init-solution problem "rust")
       (with-temp-buffer
         (insert-file-contents manifest)
         (should (string-match-p "path = \"solution.rs\"" (buffer-string))))))))

(ert-deftest +cf-init-solution/preserves-user-cargo-manifest ()
  "A non-blank user Cargo.toml is never rewritten."
  (+cf-ws-test--with-home
   (lambda ()
     (let* ((problem '(:contestId 8 :index "C"))
            (sol (+cf-init-solution problem "rust"))
            (manifest (expand-file-name "Cargo.toml" (file-name-directory sol)))
            (custom "[package]\nname = \"mine\"\nversion = \"1.0.0\"\n"))
       (with-temp-file manifest
         (insert custom))
       (+cf-init-solution problem "rust")
       (with-temp-buffer
         (insert-file-contents manifest)
         (should (equal (buffer-string) custom)))))))
