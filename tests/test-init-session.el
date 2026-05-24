;;; test-init-session.el --- Tests for init-session -*- lexical-binding: t; -*-

(require 'ert)
(require 'init-config)
(require 'init-session)
(require 'keybindings-engine)

(ert-deftest rivenEmacs-session-test-directory-setup ()
  (should (stringp rivenEmacs-session-directory))
  (should (file-name-absolute-p rivenEmacs-session-directory)))

(ert-deftest rivenEmacs-session-test-customization-types ()
  (should (boundp 'rivenEmacs-session-directory))
  (should (boundp 'rivenEmacs-session-auto-save-interval))
  (should (boundp 'rivenEmacs-session-save-geometry))
  (should (boundp 'rivenEmacs-session-restore-eager))
  (should (stringp rivenEmacs-session-directory))
  (should (or (integerp rivenEmacs-session-auto-save-interval)
              (null rivenEmacs-session-auto-save-interval)))
  (should (booleanp rivenEmacs-session-save-geometry))
  (should (or (integerp rivenEmacs-session-restore-eager)
              (eq rivenEmacs-session-restore-eager t))))

(ert-deftest rivenEmacs-session-test-helper-functions-exist ()
  (should (fboundp 'rivenEmacs-session-ensure-directory))
  (should (fboundp 'rivenEmacs-session-file))
  (should (fboundp 'rivenEmacs-session-save))
  (should (fboundp 'rivenEmacs-session-load))
  (should (fboundp 'rivenEmacs-session-reload))
  (should (fboundp 'rivenEmacs-session-clear))
  (should (fboundp 'rivenEmacs-session-delete))
  (should (fboundp 'rivenEmacs-session-current)))

(ert-deftest rivenEmacs-session-test-desktop-backend ()
  (should (featurep 'desktop))
  (should (default-value 'desktop-save-mode))
  (should (eq desktop-save t))
  (should (equal desktop-path (list rivenEmacs-session-directory)))
  (should (equal desktop-dirname rivenEmacs-session-directory))
  (should (equal desktop-auto-save-timeout rivenEmacs-session-auto-save-interval))
  (should (equal desktop-restore-frames rivenEmacs-session-save-geometry))
  (should (equal desktop-restore-eager rivenEmacs-session-restore-eager)))

(ert-deftest rivenEmacs-session-test-default-values ()
  (should (equal rivenEmacs-session-auto-save-interval 30))
  (should rivenEmacs-session-save-geometry)
  (should (equal rivenEmacs-session-restore-eager 5)))

(ert-deftest rivenEmacs-session-test-session-file ()
  (should (equal (rivenEmacs-session-file)
                 (expand-file-name desktop-base-file-name
                                   rivenEmacs-session-directory))))

(ert-deftest rivenEmacs-session-test-integration ()
  (should (featurep 'init-session))
  (should (featurep 'init-config)))

(ert-deftest rivenEmacs-session-test-has-no-private-keymap ()
  "Session commands are owned by the declarative keybinding configuration."
  (should-not (boundp 'rivenEmacs-session-map)))

(ert-deftest rivenEmacs-session-test-declarative-keybindings ()
  "Session commands are bound under `C-c x' by the declarative engine."
  (cl-letf (((symbol-function 'riven/keybindings--warn-missing-command) #'ignore))
    (riven/keybindings-apply-leader-spec))
  (dolist (entry '(("C-c x s" . rivenEmacs-session-save)
                   ("C-c x l" . rivenEmacs-session-load)
                   ("C-c x r" . rivenEmacs-session-reload)
                   ("C-c x c" . rivenEmacs-session-current)
                   ("C-c x k" . rivenEmacs-session-clear)
                   ("C-c x d" . rivenEmacs-session-delete)))
    (should (eq (key-binding (kbd (car entry))) (cdr entry)))))

(ert-deftest rivenEmacs-session-test-excludes-heavy-session-state ()
  (should (memq 'dired-mode desktop-modes-not-to-save))
  (should (memq 'help-mode desktop-modes-not-to-save))
  (should (memq 'special-mode desktop-modes-not-to-save)))

;;; test-init-session.el ends here
