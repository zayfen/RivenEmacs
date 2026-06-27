;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-auth.el --- Handle storage for Codeforces -*- lexical-binding: t; -*-

;;; Commentary:
;; Login = storing the Codeforces handle (username).  There is no cookie and no
;; network validation:
;;
;;   - The website (`/enter`, statement pages, submit form) is behind Cloudflare
;;     and unreachable from Emacs; submit + full statement are delegated to the
;;     user's browser (already logged in there).
;;   - The public JSON API (`problemset.problems`, `user.status`) needs no auth
;;     at all; `user.status` only needs the handle to know whose submissions to
;;     poll.
;;
;; So the only thing Emacs stores is the handle.  `M-x codeforces-login' writes
;; it to `~/.emacs-codeforces/credentials' and creates the workspace directory.

;;; Code:

(defvar codeforces-home-directory
  (expand-file-name "~/.emacs-codeforces/")
  "Codeforces workspace root.
Also a `defcustom' in `emacs-codeforces.el'; kept here so this module
works when loaded standalone (the `defcustom' will not overwrite an
already-set value).")

(defun +cf--credentials-file ()
  "Return the path to the credentials file under `codeforces-home-directory'."
  (expand-file-name "credentials" codeforces-home-directory))

(defun +cf--ensure-home-dir ()
  "Create `codeforces-home-directory' (and cache/workspace subdirs)."
  (make-directory (expand-file-name "cache" codeforces-home-directory) t)
  (make-directory (expand-file-name "workspace" codeforces-home-directory) t))

(defun +cf--read-credentials ()
  "Return the stored handle string, or nil if none."
  (let ((f (+cf--credentials-file)))
    (and (file-exists-p f)
         (with-temp-buffer
           (insert-file-contents f)
           (string-trim (buffer-string))))))

(defun +cf--write-credentials (handle)
  "Write HANDLE to the credentials file, mode 0600."
  (+cf--ensure-home-dir)
  (let ((f (+cf--credentials-file)))
    (with-temp-file f
      (insert handle))
    (set-file-modes f #o600)))

(defun codeforces-logged-in-p ()
  "Return non-nil if a handle is stored."
  (and (+cf--read-credentials) t))

(defun +cf--handle ()
  "Return the stored Codeforces handle, or nil."
  (+cf--read-credentials))

;;;###autoload
(defun codeforces-login (handle)
  "Store Codeforces HANDLE (your username) and create the workspace directory.
HANDLE is used only by the public API (`user.status') to poll your
submissions; no cookie is stored and no validation is done.  The browser
handles login/statement/submit (Cloudflare blocks direct Emacs access)."
  (interactive
   (list (read-string "Codeforces handle (your username, e.g. tourist): ")))
  (when (string-empty-p handle)
    (error "Handle cannot be empty"))
  (+cf--write-credentials handle)
  (+cf--ensure-home-dir)
  (message "Codeforces handle stored: %s.  Workspace: %s"
           handle codeforces-home-directory)
  handle)

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
