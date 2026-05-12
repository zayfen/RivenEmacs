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
  (expand-file-name "desktop/"
                    (if (boundp 'local-dir)
                        local-dir
                      (expand-file-name "local/" user-emacs-directory)))
  "Directory where the built-in desktop session file is stored."
  :type 'directory
  :group 'rivenEmacs)

(defcustom rivenEmacs-session-auto-save-interval
  30
  "Idle seconds before automatically saving the desktop session.
Set to nil or 0 to disable timer-based desktop autosaving."
  :type '(choice integer (const nil))
  :group 'rivenEmacs)

(defcustom rivenEmacs-session-save-geometry
  t
  "Whether to save and restore frame geometry and window state."
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
(add-to-list 'desktop-modes-not-to-save 'special-mode)

(desktop-save-mode 1)

;; ============================================================================
;; Keybindings
;; ============================================================================

(defvar rivenEmacs-session-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'rivenEmacs-session-save)
    (define-key map (kbd "l") #'rivenEmacs-session-load)
    (define-key map (kbd "r") #'rivenEmacs-session-reload)
    (define-key map (kbd "c") #'rivenEmacs-session-current)
    (define-key map (kbd "k") #'rivenEmacs-session-clear)
    (define-key map (kbd "d") #'rivenEmacs-session-delete)
    map)
  "Keymap for built-in desktop session commands under `C-c s'.")

(define-key global-map (kbd "C-c s") rivenEmacs-session-map)

;; ============================================================================
;; Provide Module
;; ============================================================================

(provide 'init-session)

;;; init-session.el ends here
