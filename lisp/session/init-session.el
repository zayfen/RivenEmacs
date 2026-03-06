;;; init-session.el --- Session management with easysession -*- lexical-binding: t; -*-

(eval-and-compile
  (let ((current-file (or load-file-name byte-compile-current-file buffer-file-name)))
    (when current-file
      (add-to-list 'load-path (file-name-directory current-file)))))

;;; Commentary:
;; Session management module for RivenEmacs using easysession.
;; Provides persistent session saving/loading including buffers,
;; window configurations, tab-bar state, and frame geometry.
;;
;; Uses easysession's built-in commands:
;; - easysession-switch-to: Switch to another session
;; - easysession-save-as: Save current session with a name
;; - easysession-save: Save current session
;; - easysession-load: Load a session (without geometry)
;; - easysession-load-including-geometry: Load with frame geometry
;; - easysession-delete: Delete a session
;; - easysession-rename: Rename current session
;; - easysession-get-current-session-name: Get current session name

;;; Code:

(require 'init-config)

;; ============================================================================
;; Customization Variables
;; ============================================================================

(defcustom rivenEmacs-session-directory
  (expand-file-name "sessions/" user-emacs-directory)
  "Directory where session files are stored."
  :type 'directory
  :group 'rivenEmacs)

(defcustom rivenEmacs-session-auto-save-interval
  (* 5 60)
  "Interval in seconds between automatic session saves.
Set to nil to disable timer-based autosaving."
  :type '(choice integer (const nil))
  :group 'rivenEmacs)

(defcustom rivenEmacs-session-default-session
  "main"
  "Default session name to load on startup."
  :type 'string
  :group 'rivenEmacs)

(defcustom rivenEmacs-session-save-geometry
  t
  "Whether to save and restore frame geometry (position and size)."
  :type 'boolean
  :group 'rivenEmacs)

(defcustom rivenEmacs-session-show-in-modeline
  t
  "Whether to display current session name in the mode line."
  :type 'boolean
  :group 'rivenEmacs)

;; ============================================================================
;; Helper Functions
;; ============================================================================

(defun rivenEmacs-session-ensure-directory ()
  "Ensure the session directory exists."
  (unless (file-directory-p rivenEmacs-session-directory)
    (make-directory rivenEmacs-session-directory t)))

(defun rivenEmacs-session-default-predicate ()
  "Default predicate for auto-saving sessions.
Returns t if the current session should be auto-saved."
  (and (boundp 'easysession--current-session-name)
       easysession--current-session-name
       (not (string-prefix-p "temp-" easysession--current-session-name))))

;; ============================================================================
;; Easysession Configuration
;; ============================================================================

(use-package easysession
  :ensure t
  :commands (easysession-switch-to
             easysession-save-as
             easysession-save
             easysession-load
             easysession-load-including-geometry
             easysession-delete
             easysession-rename
             easysession-save-mode
             easysession-get-current-session-name)

  :custom
  ;; Session storage location
  (easysession-directory rivenEmacs-session-directory)

  ;; Auto-save configuration
  (easysession-save-interval rivenEmacs-session-auto-save-interval)

  ;; Mode line display
  (easysession-mode-line-misc-info rivenEmacs-session-show-in-modeline)
  (easysession-save-mode-lighter-show-session-name t)

  ;; Predicate for auto-save
  (easysession-save-mode-predicate #'rivenEmacs-session-default-predicate)

  ;; Exclude certain hooks from running during session restore
  (easysession-find-file-exclude-hook-regexp
   "flymake\\|eldoc\\|lsp\\|eglot")

  :init
  ;; Ensure session directory exists
  (rivenEmacs-session-ensure-directory)

  ;; Load session on startup (after all other packages)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when rivenEmacs-session-default-session
                (easysession-load-including-geometry
                 rivenEmacs-session-default-session)))
            102)

  (add-hook 'easysession-before-load-hook #'easysession-reset)
  (add-hook 'easysession-new-session-hook #'easysession-reset)

  ;; Automatically save all buffers without prompting the user
  (add-hook 'easysession-before-reset-hook #'(lambda()
                                               (save-some-buffers t)))

  ;; Enable auto-save mode
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103)

  :config
  ;; Hook to ensure directory exists before saving
  (add-hook 'easysession-before-save-hook #'rivenEmacs-session-ensure-directory))

;; ============================================================================
;; Hooks for Enhanced Session Management
;; ============================================================================

(defun rivenEmacs-session-setup-empty ()
  "Set up a minimal environment when creating a new session."
  (when (and (boundp 'tab-bar-mode) tab-bar-mode)
    (tab-bar-close-other-tabs))
  (delete-other-windows)
  (scratch-buffer))

(add-hook 'easysession-new-session-hook #'rivenEmacs-session-setup-empty)

;; ============================================================================
;; Keybindings
;; ============================================================================

;; Global prefix "C-c s" for session management
;; Uses easysession's built-in commands directly
(defvar rivenEmacs-session-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'easysession-switch-to)
    (define-key map (kbd "S") #'easysession-save-as)
    (define-key map (kbd "l") #'easysession-load)
    (define-key map (kbd "L") #'easysession-load-including-geometry)
    (define-key map (kbd "d") #'easysession-delete)
    (define-key map (kbd "r") #'easysession-rename)
    (define-key map (kbd ".") #'easysession-save)
    (define-key map (kbd "c") #'easysession-get-current-session-name)
    map)
  "Keymap for session management commands under C-c s prefix.
Uses easysession's built-in commands.

\<rivenEmacs-session-map>
\[easysession-switch-to] - Switch to session
\[easysession-save-as] - Save as new session
\[easysession-load] - Load session (without geometry)
\[easysession-load-including-geometry] - Load with geometry
\[easysession-delete] - Delete session
\[easysession-rename] - Rename session
\[easysession-save] - Save current session
\[easysession-get-current-session-name] - Show current session")

;; Bind the prefix map to C-c s
(define-key global-map (kbd "C-c s") rivenEmacs-session-map)


;; ============================================================================
;; Provide Module
;; ============================================================================

(provide 'init-session)

;;; init-session.el ends here
