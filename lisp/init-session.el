;;; init-session.el --- Session management with easysession -*- lexical-binding: t; -*-

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
  (expand-file-name "local/sessions/" user-emacs-directory)
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

;; Also add leader key bindings for those using SPC prefix
(with-eval-after-load 'init-keybindings
  (when (fboundp 'leader-def)
    (leader-def
      :infix "s"
      "" '(:ignore t :wk "Session")
      "s" '(easysession-switch-to :wk "Switch session")
      "S" '(easysession-save-as :wk "Save as new")
      "l" '(easysession-load :wk "Load session")
      "L" '(easysession-load-including-geometry :wk "Load with geometry")
      "d" '(easysession-delete :wk "Delete session")
      "r" '(easysession-rename :wk "Rename session")
      "." '(easysession-save :wk "Save current")
      "c" '(easysession-get-current-session-name :wk "Current session"))))

;; ============================================================================
;; Provide Module
;; ============================================================================

(provide 'init-session)

;;; init-session.el ends here

;; ============================================================================
;; Unit Tests
;; ============================================================================

(ert-deftest rivenEmacs-session-test-directory-setup ()
  "Test that session directory is properly set up."
  (should (stringp rivenEmacs-session-directory))
  (should (file-name-absolute-p rivenEmacs-session-directory)))

(ert-deftest rivenEmacs-session-test-customization-types ()
  "Test that customization variables have correct types."
  ;; Test that variables are defined
  (should (boundp 'rivenEmacs-session-directory))
  (should (boundp 'rivenEmacs-session-auto-save-interval))
  (should (boundp 'rivenEmacs-session-default-session))
  (should (boundp 'rivenEmacs-session-save-geometry))
  (should (boundp 'rivenEmacs-session-show-in-modeline))

  ;; Test types
  (should (stringp rivenEmacs-session-directory))
  (should (or (integerp rivenEmacs-session-auto-save-interval)
              (null rivenEmacs-session-auto-save-interval)))
  (should (stringp rivenEmacs-session-default-session))
  (should (booleanp rivenEmacs-session-save-geometry))
  (should (booleanp rivenEmacs-session-show-in-modeline)))

(ert-deftest rivenEmacs-session-test-helper-functions-exist ()
  "Test that helper functions are defined."
  (should (fboundp 'rivenEmacs-session-ensure-directory))
  (should (fboundp 'rivenEmacs-session-default-predicate))
  (should (fboundp 'rivenEmacs-session-setup-empty)))

(ert-deftest rivenEmacs-session-test-predicate ()
  "Test session save predicate."
  ;; Test that predicate returns nil when no session is loaded
  (let ((easysession--current-session-name nil))
    (should-not (rivenEmacs-session-default-predicate)))

  ;; Test that predicate returns t for normal sessions
  (let ((easysession--current-session-name "main"))
    (should (rivenEmacs-session-default-predicate)))

  ;; Test that predicate returns nil for temp sessions
  (let ((easysession--current-session-name "temp-test"))
    (should-not (rivenEmacs-session-default-predicate))))

(ert-deftest rivenEmacs-session-test-easysession-commands-available ()
  "Test that easysession commands are properly configured."
  ;; These should be autoloaded commands
  (should (fboundp 'easysession-switch-to))
  (should (fboundp 'easysession-save-as))
  (should (fboundp 'easysession-save))
  (should (fboundp 'easysession-load))
  (should (fboundp 'easysession-load-including-geometry))
  (should (fboundp 'easysession-delete))
  (should (fboundp 'easysession-rename))
  (should (fboundp 'easysession-save-mode))
  (should (fboundp 'easysession-get-current-session-name)))

(ert-deftest rivenEmacs-session-test-default-values ()
  "Test default configuration values."
  (should (equal rivenEmacs-session-default-session "main"))
  (should (equal rivenEmacs-session-auto-save-interval 300))
  (should rivenEmacs-session-save-geometry)
  (should rivenEmacs-session-show-in-modeline))

(ert-deftest rivenEmacs-session-test-integration ()
  "Test module integration with RivenEmacs."
  ;; Test that module provides itself
  (should (featurep 'init-session))

  ;; Test that init-config is required
  (should (featurep 'init-config)))

(ert-deftest rivenEmacs-session-test-keymap-exists ()
  "Test that session keymap is defined."
  (should (boundp 'rivenEmacs-session-map))
  (should (keymapp rivenEmacs-session-map)))

(ert-deftest rivenEmacs-session-test-global-keybinding ()
  "Test that C-c s prefix is bound."
  (let ((binding (lookup-key global-map (kbd "C-c s"))))
    (should binding)
    (should (keymapp binding))))

(ert-deftest rivenEmacs-session-test-keymap-bindings ()
  "Test that session keymap binds easysession commands directly."
  (let ((map rivenEmacs-session-map))
    ;; All these keys should be bound
    (should (lookup-key map (kbd "s")))
    (should (lookup-key map (kbd "S")))
    (should (lookup-key map (kbd "l")))
    (should (lookup-key map (kbd "L")))
    (should (lookup-key map (kbd "d")))
    (should (lookup-key map (kbd "r")))
    (should (lookup-key map (kbd ".")))
    (should (lookup-key map (kbd "c")))

    ;; Check that easysession functions are bound directly
    (should (eq (lookup-key map (kbd "s")) #'easysession-switch-to))
    (should (eq (lookup-key map (kbd "S")) #'easysession-save-as))
    (should (eq (lookup-key map (kbd "l")) #'easysession-load))
    (should (eq (lookup-key map (kbd "L")) #'easysession-load-including-geometry))
    (should (eq (lookup-key map (kbd "d")) #'easysession-delete))
    (should (eq (lookup-key map (kbd "r")) #'easysession-rename))
    (should (eq (lookup-key map (kbd ".")) #'easysession-save))
    (should (eq (lookup-key map (kbd "c")) #'easysession-get-current-session-name))))

(ert-deftest rivenEmacs-session-test-hooks-registered ()
  "Test that session hooks are properly registered."
  ;; Test new session hook
  (should (member #'rivenEmacs-session-setup-empty easysession-new-session-hook))
  ;; Test startup hooks are configured
  (should (member 'easysession-save-mode emacs-startup-hook)))

;; Run tests automatically if ert-runner is not being used
(when (and (not noninteractive)
           (not (getenv "ERT_RUNNER")))
  (message "RivenEmacs session module loaded. Run M-x ert RET t RET to run tests."))
