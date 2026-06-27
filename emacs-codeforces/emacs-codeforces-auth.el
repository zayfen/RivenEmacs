;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-auth.el --- Session-cookie login for Codeforces -*- lexical-binding: t; -*-

;;; Commentary:
;; Login is done by injecting a browser session cookie (Codeforces /enter has
;; hCaptcha, so programmatic username/password login is not feasible).
;;
;; Flow: `codeforces-login' prompts for the cookie string, writes it to
;; `credentials' (chmod 600), validates by GETting /enter and detecting the
;; "Logout" link, and creates `codeforces-home-directory' on success.

;;; Code:

(require 'emacs-codeforces-client)

;; Declared as `defcustom' in emacs-codeforces.el; defvar here so this module
;; can be loaded and tested standalone.
(defvar codeforces-home-directory nil
  "Codeforces workspace root.  Set by `emacs-codeforces.el'.")

(defun +cf--home-dir ()
  "Return the codeforces home directory (as configured)."
  codeforces-home-directory)

(defun +cf--credentials-file ()
  "Return the path to the credentials file under `codeforces-home-directory'."
  (expand-file-name "credentials" (+cf--home-dir)))

(defun +cf--login-page-logged-in-p (html)
  "Return non-nil if the /enter page HTML indicates an active session.
We detect the Logout link that only appears when authenticated."
  (string-match-p "Logout" (or html "")))

(defun +cf--ensure-home-dir ()
  "Create `codeforces-home-directory' (and cache/workspace subdirs)."
  (make-directory (expand-file-name "cache" codeforces-home-directory) t)
  (make-directory (expand-file-name "workspace" codeforces-home-directory) t))

(defun +cf--read-credentials ()
  "Return the stored cookie string, or nil if none."
  (let ((f (+cf--credentials-file)))
    (and (file-exists-p f)
         (with-temp-buffer
           (insert-file-contents f)
           (string-trim (buffer-string))))))

(defun +cf--write-credentials (cookie)
  "Write COOKIE to the credentials file, mode 0600."
  (+cf--ensure-home-dir)
  (let ((f (+cf--credentials-file)))
    (with-temp-file f
      (insert cookie)
      (set-file-modes f #o600))))

(defun codeforces-logged-in-p ()
  "Return non-nil if a credential file exists (does not validate freshness)."
  (and (+cf--read-credentials) t))

(defun +cf--cookie-header ()
  "Return the current cookie as a Cookie header value, or nil."
  (+cf--read-credentials))

;;;###autoload
(defun codeforces-login (cookie)
  "Log in to Codeforces using a browser session COOKIE string.
COOKIE is validated by fetching the login page; on success the home directory
is created.  Interactively, prompts for the cookie (default: current kill)."
  (interactive
   (list (read-string "Codeforces session cookie (paste from browser): "
                      (current-kill 0 t))))
  (message "Validating Codeforces cookie...")
  (+cf--write-credentials cookie)
  (let* ((resp (+cf-http-get (concat +cf-site-base "enter")
                             :cookie cookie))
         (status (car resp))
         (body (cdr resp)))
    (if (and (= status 200) (+cf--login-page-logged-in-p body))
        (progn
          (+cf--ensure-home-dir)
          (message "Codeforces login OK.  Workspace: %s" codeforces-home-directory)
          t)
      (+cf--write-credentials "")        ; invalidate
      (error "Codeforces login failed (status %s). Check your cookie." status))))

;;;###autoload
(defun codeforces-logout ()
  "Clear stored Codeforces credentials."
  (interactive)
  (let ((f (+cf--credentials-file)))
    (when (file-exists-p f)
      (delete-file f)))
  (message "Codeforces credentials cleared."))

(provide 'emacs-codeforces-auth)

;;; emacs-codeforces-auth.el ends here
