;;; init-session.el --- Session management with desktop.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Session management module for RivenEmacs using Emacs' built-in desktop.el.
;; It persists buffers, point, window/frame state, and selected global history
;; without the named session index maintained by external session packages.

;;; Code:

(require 'init-config)
(require 'desktop)

;; ============================================================================
;; Customization Variables
;; ============================================================================

(defcustom rivenEmacs-session-directory
  (expand-file-name "emacs/" "~/.cache/")
  "Directory where the built-in desktop session file is stored.
The default is the fixed cache directory ~/.cache/emacs/ so desktop
sessions are shared by all RivenEmacs instances and are never stored
inside a project directory."
  :type 'directory
  :group 'rivenEmacs)

(defcustom rivenEmacs-session-auto-save-interval
  30
  "Idle seconds before automatically saving the desktop session.
Set to nil or 0 to disable timer-based desktop autosaving."
  :type '(choice integer (const nil))
  :group 'rivenEmacs)

(defcustom rivenEmacs-session-save-geometry
  nil
  "Whether to save and restore frame geometry and window state.
Keep this nil by default so desktop restore does not create extra Emacs frames
on startup."
  :type 'boolean
  :group 'rivenEmacs)

(defcustom rivenEmacs-session-restore-eager
  5
  "Number of buffers to restore immediately before lazy restoration.
Set to t to restore all buffers eagerly."
  :type '(choice integer (const t))
  :group 'rivenEmacs)

;; ============================================================================
;; Helper Functions
;; ============================================================================

(defun rivenEmacs-session-ensure-directory ()
  "Ensure `rivenEmacs-session-directory' exists."
  (unless (file-directory-p rivenEmacs-session-directory)
    (make-directory rivenEmacs-session-directory t)))

(defun rivenEmacs-session-file ()
  "Return the full path of the active desktop session file."
  (expand-file-name desktop-base-file-name rivenEmacs-session-directory))

(defconst rivenEmacs-session-legacy-directory
  (expand-file-name "desktop/"
                    (if (boundp 'local-dir)
                        local-dir
                      (expand-file-name "local/" user-emacs-directory)))
  "Old per-repository desktop directory, used for one-time migration.")

(defun rivenEmacs-session-migrate-legacy ()
  "Copy a legacy local/desktop session into the fixed cache directory."
  (let ((target (rivenEmacs-session-file))
        (legacy (expand-file-name desktop-base-file-name
                                  rivenEmacs-session-legacy-directory)))
    (when (and (not (file-exists-p target))
               (file-exists-p legacy)
               (not (equal (file-truename rivenEmacs-session-directory)
                           (file-truename rivenEmacs-session-legacy-directory))))
      (rivenEmacs-session-ensure-directory)
      (copy-file legacy target t)
      (message "Migrated desktop session from %s to %s"
               (abbreviate-file-name legacy)
               (abbreviate-file-name target)))))

(defun rivenEmacs-session-handle-desktop-read ()
  "Keep the desktop directory fixed after `desktop-read'.
If another Emacs instance owns the desktop lock, or a desktop file
exists but was not loaded, disable saving for this instance instead of
letting `desktop-kill' prompt for a directory on exit."
  (unless desktop-dirname
    (setq desktop-dirname rivenEmacs-session-directory)
    (let ((owner (ignore-errors (desktop-owner))))
      (cond
       ((and owner (/= owner (emacs-pid)))
        (setq desktop-save-mode nil)
        (message "Desktop session is in use by Emacs PID %d; this instance will not save it."
                 owner))
       ((file-exists-p (rivenEmacs-session-file))
        (setq desktop-save-mode nil)
        (message "Desktop session file exists but was not loaded; this instance will not overwrite it."))))))

(defun rivenEmacs-session-prepare-desktop-kill (&rest _)
  "Never ask `desktop-kill' for a directory; use the fixed cache directory."
  (unless desktop-dirname
    (setq desktop-dirname rivenEmacs-session-directory)
    (let ((owner (ignore-errors (desktop-owner))))
      (when (and owner (/= owner (emacs-pid)))
        (setq desktop-save-mode nil)))))

(advice-add 'desktop-kill :before #'rivenEmacs-session-prepare-desktop-kill)

(defun rivenEmacs-session-save ()
  "Save the current desktop session."
  (interactive)
  (rivenEmacs-session-ensure-directory)
  (desktop-save rivenEmacs-session-directory)
  (message "Desktop session saved in %s"
           (abbreviate-file-name rivenEmacs-session-directory)))

(defun rivenEmacs-session-load ()
  "Load the desktop session from `rivenEmacs-session-directory'."
  (interactive)
  (rivenEmacs-session-ensure-directory)
  (if (file-exists-p (rivenEmacs-session-file))
      (desktop-read rivenEmacs-session-directory)
    (message "No desktop session file found in %s"
             (abbreviate-file-name rivenEmacs-session-directory))))

(defun rivenEmacs-session-reload ()
  "Clear current buffers and reload the desktop session."
  (interactive)
  (desktop-clear)
  (rivenEmacs-session-load))

(defun rivenEmacs-session-clear ()
  "Clear the current desktop without deleting the saved session file."
  (interactive)
  (desktop-clear)
  (message "Desktop session cleared"))

(defun rivenEmacs-session-delete ()
  "Delete the saved desktop session file."
  (interactive)
  (let ((session-file (rivenEmacs-session-file)))
    (when (or (not (file-exists-p session-file))
              (yes-or-no-p (format "Delete desktop session %s? "
                                   (abbreviate-file-name session-file))))
      (when (file-exists-p session-file)
        (delete-file session-file))
      (desktop-release-lock rivenEmacs-session-directory)
      (when (and desktop-dirname
                 (equal (file-name-as-directory (expand-file-name rivenEmacs-session-directory))
                        (file-name-as-directory (expand-file-name desktop-dirname))))
        (setq desktop-dirname nil))
      (message "Desktop session deleted"))))

(defun rivenEmacs-session-current ()
  "Show the active desktop session file."
  (interactive)
  (message "Desktop session: %s"
           (abbreviate-file-name
            (or (and desktop-dirname (desktop-full-file-name))
                (rivenEmacs-session-file)))))

;; ============================================================================
;; Desktop Configuration
;; ============================================================================

(rivenEmacs-session-ensure-directory)
(rivenEmacs-session-migrate-legacy)

(setq desktop-dirname rivenEmacs-session-directory
      desktop-path (list rivenEmacs-session-directory)
      desktop-save t
      desktop-auto-save-timeout rivenEmacs-session-auto-save-interval
      desktop-restore-frames rivenEmacs-session-save-geometry
      desktop-restore-eager rivenEmacs-session-restore-eager
      desktop-load-locked-desktop 'check-pid
      desktop-lazy-verbose nil
      desktop-files-not-to-save
      (rx (or
           (: string-start "/" (* (not (any "/:"))) ":")
           (: "/"
              (or ".cache" ".git" "cache" "eln-cache" "node_modules")
              (* anychar))
           (: (or "/rsync" "/ssh" "/sudo" "/sudoedit" "/tmp" "/yadm")
              (* anychar))))
      desktop-buffers-not-to-save
      (rx string-start
          (or " "
              "*Completions*"
              "*Compile-Log*"
              "*Flymake"
              "*Messages*"
              "*Warnings*")))

(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'help-mode)

(desktop-save-mode 1)

;; desktop.el installs its own `desktop-read' on `after-init-hook' (depth 0).
;; Run after it so a locked desktop cannot leave `desktop-dirname' nil and
;; trigger the "Directory for desktop file" prompt at exit.
(add-hook 'after-init-hook #'rivenEmacs-session-handle-desktop-read 90)

;; ============================================================================
;; Provide Module
;; ============================================================================

(provide 'init-session)

;;; init-session.el ends here
