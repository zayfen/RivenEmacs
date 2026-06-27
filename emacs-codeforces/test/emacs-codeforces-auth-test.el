;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'ert)
(require 'emacs-codeforces-auth)

(ert-deftest +cf--credentials-file/under-home ()
  "Credentials file lives under the configured home directory."
  (let ((codeforces-home-directory "/tmp/cf/"))
    (should (equal (+cf--credentials-file) "/tmp/cf/credentials"))))

(ert-deftest codeforces-login/stores-handle-and-creates-home ()
  "Login writes the handle to credentials and creates the workspace dir."
  (let ((codeforces-home-directory
         (expand-file-name (make-temp-name "cf-auth-") temporary-file-directory)))
    (unwind-protect
        (progn
          (codeforces-login "tourist")
          (should (equal (+cf--handle) "tourist"))
          (should (file-directory-p
                   (expand-file-name "workspace" codeforces-home-directory)))
          (should (file-directory-p
                   (expand-file-name "cache" codeforces-home-directory))))
      (delete-directory codeforces-home-directory t))))

(ert-deftest codeforces-login/empty-handle-errors ()
  "An empty handle is rejected."
  (let ((codeforces-home-directory
         (expand-file-name (make-temp-name "cf-auth-") temporary-file-directory)))
    (unwind-protect
        (should-error (codeforces-login ""))
      (delete-directory codeforces-home-directory t))))

(ert-deftest codeforces-logout/clears-credentials ()
  "Logout removes the credentials file."
  (let ((codeforces-home-directory
         (expand-file-name (make-temp-name "cf-auth-") temporary-file-directory)))
    (unwind-protect
        (progn
          (codeforces-login "tourist")
          (should (codeforces-logged-in-p))
          (codeforces-logout)
          (should-not (codeforces-logged-in-p))
          (should (null (+cf--handle))))
      (delete-directory codeforces-home-directory t))))
