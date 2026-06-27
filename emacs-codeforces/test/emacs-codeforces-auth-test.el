;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'ert)
(require 'emacs-codeforces-auth)

(ert-deftest +cf--credentials-file/under-home ()
  "Credentials file lives under the configured home directory."
  (let ((codeforces-home-directory "/tmp/cf/"))
    (should (equal (+cf--credentials-file) "/tmp/cf/credentials"))))

(ert-deftest +cf--cookie-file/under-home ()
  "Cookie file lives under the configured home directory."
  (let ((codeforces-home-directory "/tmp/cf/"))
    (should (equal (+cf--cookie-file) "/tmp/cf/cookie"))))

(ert-deftest codeforces-login/stores-handle-and-cookie ()
  "Login writes handle+cookie and creates the workspace dir."
  (let ((codeforces-home-directory
         (expand-file-name (make-temp-name "cf-auth-") temporary-file-directory)))
    (unwind-protect
        (progn
          (codeforces-login "tourist" "JSESSIONID=abc; session=xyz")
          (should (equal (+cf--handle) "tourist"))
          (should (equal (+cf--cookie) "JSESSIONID=abc; session=xyz"))
          ;; cookie file (read by the agent) contains the cookie
          (should (file-exists-p (+cf--cookie-file)))
          (should (string-match-p "session=xyz"
                                  (with-temp-buffer
                                    (insert-file-contents (+cf--cookie-file))
                                    (buffer-string))))
          (should (file-directory-p
                   (expand-file-name "workspace" codeforces-home-directory))))
      (delete-directory codeforces-home-directory t))))

(ert-deftest codeforces-login/empty-handle-errors ()
  "An empty handle is rejected."
  (let ((codeforces-home-directory
         (expand-file-name (make-temp-name "cf-auth-") temporary-file-directory)))
    (unwind-protect
        (should-error (codeforces-login "" "some-cookie"))
      (delete-directory codeforces-home-directory t))))

(ert-deftest codeforces-login/empty-cookie-errors ()
  "An empty cookie is rejected."
  (let ((codeforces-home-directory
         (expand-file-name (make-temp-name "cf-auth-") temporary-file-directory)))
    (unwind-protect
        (should-error (codeforces-login "tourist" ""))
      (delete-directory codeforces-home-directory t))))

(ert-deftest codeforces-logout/clears-credentials ()
  "Logout removes both credentials and cookie files."
  (let ((codeforces-home-directory
         (expand-file-name (make-temp-name "cf-auth-") temporary-file-directory)))
    (unwind-protect
        (progn
          (codeforces-login "tourist" "session=xyz")
          (should (codeforces-logged-in-p))
          (codeforces-logout)
          (should-not (codeforces-logged-in-p))
          (should (null (+cf--handle)))
          (should-not (file-exists-p (+cf--cookie-file))))
      (delete-directory codeforces-home-directory t))))
