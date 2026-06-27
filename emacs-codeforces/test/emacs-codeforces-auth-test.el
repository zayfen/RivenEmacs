;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'ert)
(require 'emacs-codeforces-auth)

(ert-deftest +cf--login-page-logged-in-p/detects-logout-link ()
  "A page containing the logout link is logged-in."
  (should (+cf--login-page-logged-in-p
           "<a href='/'>Logout</a><form>...</form>")))

(ert-deftest +cf--login-page-logged-in-p/login-form-is-not-logged-in ()
  "A page with only the login form is not logged-in."
  (should-not
   (+cf--login-page-logged-in-p
    "<form action='/enter' method='post'><input name='handleOrEmail'>")))

(ert-deftest +cf--credentials-file/under-home ()
  "Credentials file lives under the configured home directory."
  (let ((codeforces-home-directory "/tmp/cf/"))
    (should (equal (+cf--credentials-file) "/tmp/cf/credentials"))))
