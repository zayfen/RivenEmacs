;;; test-init-session.el --- Tests for init-session -*- lexical-binding: t; -*-

(require 'ert)
(require 'init-config)
(require 'init-session)

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

(ert-deftest rivenEmacs-session-test-keymap-exists ()
  (should (boundp 'rivenEmacs-session-map))
  (should (keymapp rivenEmacs-session-map)))

(ert-deftest rivenEmacs-session-test-global-keybinding ()
  (let ((binding (lookup-key global-map (kbd "C-c s"))))
    (should binding)
    (should (keymapp binding))))

(ert-deftest rivenEmacs-session-test-keymap-bindings ()
  (let ((map rivenEmacs-session-map))
    (should (eq (lookup-key map (kbd "s")) #'rivenEmacs-session-save))
    (should (eq (lookup-key map (kbd "l")) #'rivenEmacs-session-load))
    (should (eq (lookup-key map (kbd "r")) #'rivenEmacs-session-reload))
    (should (eq (lookup-key map (kbd "c")) #'rivenEmacs-session-current))
    (should (eq (lookup-key map (kbd "k")) #'rivenEmacs-session-clear))
    (should (eq (lookup-key map (kbd "d")) #'rivenEmacs-session-delete))))

(ert-deftest rivenEmacs-session-test-excludes-heavy-session-state ()
  (should (memq 'dired-mode desktop-modes-not-to-save))
  (should (memq 'help-mode desktop-modes-not-to-save))
  (should (memq 'special-mode desktop-modes-not-to-save)))

;;; test-init-session.el ends here
