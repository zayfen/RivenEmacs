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

;;; Private credential storage

(defmacro +cf-auth-test--with-temp-home (&rest body)
  "Evaluate BODY with a fresh private `codeforces-home-directory'.
The temporary directory is removed afterwards."
  (declare (indent 0) (debug t))
  `(let ((codeforces-home-directory
          (expand-file-name (make-temp-name "cf-auth-") temporary-file-directory)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p codeforces-home-directory)
         (delete-directory codeforces-home-directory t)))))

(defun +cf-auth-test--file-modes (file)
  "Return the permission bits of FILE as an integer."
  (logand (file-modes file) #o777))

(ert-deftest codeforces-login/writes-private-credentials ()
  "Login creates 0600 credential/cookie files in a 0700 home directory."
  (+cf-auth-test--with-temp-home
    (codeforces-login "tourist" "session=xyz")
    (should (= (+cf-auth-test--file-modes codeforces-home-directory) #o700))
    (dolist (file (list (+cf--credentials-file) (+cf--cookie-file)))
      (should (file-exists-p file))
      (should (= (+cf-auth-test--file-modes file) #o600)))
    ;; The atomic write must not leave temporary files behind.
    (should-not (directory-files codeforces-home-directory nil "\\`\\.cf-tmp-"))
    (should (file-directory-p (expand-file-name "workspace" codeforces-home-directory)))
    (should (file-directory-p (expand-file-name "cache" codeforces-home-directory)))))

(ert-deftest codeforces-login/cookie-prompt-avoids-minibuffer-history ()
  "The interactive cookie prompt must not record the cookie in history.
The cookie is read with `read-passwd' (hidden input, no history) and the
only `read-string' prompt is the non-secret handle."
  (let* ((spec (format "%S" (interactive-form 'codeforces-login)))
         (read-string-count
          (let ((count 0) (position 0))
            (while (string-match "read-string" spec position)
              (setq count (1+ count)
                    position (match-end 0)))
            count)))
    (should (string-match-p "read-passwd" spec))
    (should (= read-string-count 1))
    (should (string-match-p "+cf--cookie-history" spec))
    (should (boundp '+cf--cookie-history))))

(ert-deftest codeforces-logout/clears-cookie-history-and-files ()
  "Logout removes the credential files and the private cookie history."
  (+cf-auth-test--with-temp-home
    (codeforces-login "tourist" "session=xyz")
    (setq +cf--cookie-history '("stale-cookie"))
    (codeforces-logout)
    (should-not (file-exists-p (+cf--credentials-file)))
    (should-not (file-exists-p (+cf--cookie-file)))
    (should-not (codeforces-logged-in-p))
    (should-not +cf--cookie-history)))
