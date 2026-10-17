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

(defvar +cf--cookie-history nil
  "Private minibuffer history for Codeforces session cookies.
Kept out of `minibuffer-history' so `savehist-mode' never persists a
session cookie to disk.")

(defun +cf--credentials-file ()
  "Return the path to the credentials file (handle=.../cookie=...)."
  (expand-file-name "credentials" codeforces-home-directory))

(defun +cf--cookie-file ()
  "Return the path to the cookie file read by the Python agent."
  (expand-file-name "cookie" codeforces-home-directory))

(defun +cf--ensure-home-dir ()
  "Create `codeforces-home-directory' (and cache/workspace subdirs).
The home directory itself is made private (mode 0700) because it holds
the session cookie."
  (make-directory (expand-file-name "cache" codeforces-home-directory) t)
  (make-directory (expand-file-name "workspace" codeforces-home-directory) t)
  (set-file-modes codeforces-home-directory #o700))

(defun +cf--write-private-file (file content)
  "Atomically write CONTENT to FILE with private (0600) permissions.
The data is first written to a temporary file in FILE's own directory
and then renamed into place, so a reader never observes a partial file
and FILE is created private instead of being chmod'ed after the fact."
  (let* ((dir (file-name-directory file))
         (tmp (make-temp-file (expand-file-name ".cf-tmp-" dir))))
    (unwind-protect
        (let ((coding-system-for-write 'utf-8-unix))
          (with-temp-file tmp
            (insert content))
          (rename-file tmp file t))
      (when (file-exists-p tmp)
        (delete-file tmp)))))

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
  "Write HANDLE and COOKIE to the credentials and cookie files (mode 0600).
Both files are written atomically with a private temporary file, so the
credentials never exist with looser permissions and are never partially
visible to readers."
  (+cf--ensure-home-dir)
  (+cf--write-private-file (+cf--credentials-file)
                           (format "handle=%s\ncookie=%s\n" handle cookie))
  (+cf--write-private-file (+cf--cookie-file) cookie))

;;;###autoload
(defun codeforces-login (handle cookie)
  "Store Codeforces HANDLE (username) and session COOKIE, then create workspace.
COOKIE is the full Cookie header value copied from an authenticated browser
request to codeforces.com (used by the agent to submit).  No validation is
done here.  Interactively, prompts for both."
  (interactive
   (list (read-string "Codeforces handle (your username, e.g. tourist): ")
         ;; `read-passwd' hides the typed input and never records it in the
         ;; default `minibuffer-history'; the private history binding is a
         ;; second line of defence for prompts that do record history.
         (let ((minibuffer-history-variable '+cf--cookie-history))
           (unwind-protect
               (read-passwd "Codeforces session cookie (full Cookie: header from browser): ")
             (setq +cf--cookie-history nil)))))
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
  (setq +cf--cookie-history nil)
  (message "Codeforces credentials cleared."))

(provide 'emacs-codeforces-auth)

;;; emacs-codeforces-auth.el ends here
