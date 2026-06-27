;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-auth.el --- Handle + cookie storage for Codeforces -*- lexical-binding: t; -*-

;;; Commentary:
;; Login = storing the Codeforces handle (username) and a browser session
;; cookie.  The cookie is consumed by the Python agent (codeforces_agent.py)
;; to submit solutions; the handle is used by the public API (user.status) to
;; poll verdicts.  No validation is done here — the website is Cloudflare-
;; gated, so any /enter-based check would always report invalid.  Correctness
;; is established when the agent first uses the cookie (submit) or the API
;; first uses the handle (poll).

;;; Code:

(defvar codeforces-home-directory
  (expand-file-name "~/.emacs-codeforces/")
  "Codeforces workspace root.
Also a `defcustom' in `emacs-codeforces.el'; kept here so this module
works when loaded standalone (the `defcustom' will not overwrite an
already-set value).")

(defun +cf--credentials-file ()
  "Return the path to the credentials file (handle=.../cookie=...)."
  (expand-file-name "credentials" codeforces-home-directory))

(defun +cf--cookie-file ()
  "Return the path to the cookie file read by the Python agent."
  (expand-file-name "cookie" codeforces-home-directory))

(defun +cf--ensure-home-dir ()
  "Create `codeforces-home-directory' (and cache/workspace subdirs)."
  (make-directory (expand-file-name "cache" codeforces-home-directory) t)
  (make-directory (expand-file-name "workspace" codeforces-home-directory) t))

(defun +cf--parse-credentials ()
  "Return an alist ((handle . \"...\") (cookie . \"...\")) from the credentials file."
  (let ((f (+cf--credentials-file)))
    (if (file-exists-p f)
        (with-temp-buffer
          (insert-file-contents f)
          (let ((alist nil))
            (goto-char (point-min))
            (while (re-search-forward "^\\([a-z]+\\)=\\(.*\\)$" nil t)
              (push (cons (intern (match-string 1))
                          (string-trim (match-string 2)))
                    alist))
            alist))
      nil)))

(defun +cf--read-credentials ()
  "Return non-nil if credentials exist (does not validate contents)."
  (file-exists-p (+cf--credentials-file)))

(defun codeforces-logged-in-p ()
  "Return non-nil if credentials are stored."
  (and (+cf--read-credentials) t))

(defun +cf--handle ()
  "Return the stored Codeforces handle, or nil."
  (alist-get 'handle (+cf--parse-credentials)))

(defun +cf--cookie ()
  "Return the stored cookie string, or nil."
  (alist-get 'cookie (+cf--parse-credentials)))

(defun +cf--write-credentials (handle cookie)
  "Write HANDLE and COOKIE to the credentials and cookie files (mode 0600)."
  (+cf--ensure-home-dir)
  (let ((creds (+cf--credentials-file))
        (cookief (+cf--cookie-file)))
    (with-temp-file creds
      (insert (format "handle=%s\ncookie=%s\n" handle cookie)))
    (set-file-modes creds #o600)
    (with-temp-file cookief
      (insert cookie))
    (set-file-modes cookief #o600)))

;;;###autoload
(defun codeforces-login (handle cookie)
  "Store Codeforces HANDLE (username) and session COOKIE, then create workspace.
COOKIE is the full Cookie header value copied from an authenticated browser
request to codeforces.com (used by the agent to submit).  No validation is
done here.  Interactively, prompts for both."
  (interactive
   (list (read-string "Codeforces handle (your username, e.g. tourist): ")
         (read-string "Codeforces session cookie (full Cookie: header from browser): ")))
  (when (string-empty-p handle)
    (error "Handle cannot be empty"))
  (when (string-empty-p cookie)
    (error "Cookie cannot be empty"))
  (+cf--write-credentials handle cookie)
  (+cf--ensure-home-dir)
  (message "Codeforces login stored.  Handle: %s  Workspace: %s"
           handle codeforces-home-directory)
  handle)

;;;###autoload
(defun codeforces-logout ()
  "Clear stored Codeforces credentials and cookie."
  (interactive)
  (dolist (f (list (+cf--credentials-file) (+cf--cookie-file)))
    (when (file-exists-p f)
      (delete-file f)))
  (message "Codeforces credentials cleared."))

(provide 'emacs-codeforces-auth)

;;; emacs-codeforces-auth.el ends here
