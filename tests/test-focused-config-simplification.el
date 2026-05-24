;;; test-focused-config-simplification.el --- Focused wiring tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-keybindings)

(defconst riven/test-repo-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root used by focused configuration tests.")

(defun riven/test-read-repo-file (relative-path)
  "Return the contents of RELATIVE-PATH below `riven/test-repo-root'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name relative-path riven/test-repo-root))
    (buffer-string)))

(defmacro riven/test-with-quiet-keybinding-diagnostics (&rest body)
  "Evaluate BODY without missing-command messages from unloaded packages."
  `(cl-letf (((symbol-function 'riven/keybindings--warn-missing-command) #'ignore))
     ,@body))

(ert-deftest riven/keybindings-config-does-not-require-agent-shell ()
  "Setting keybindings must not load agent-shell eagerly."
  (let ((agent-shell-requested nil)
        (original-require (symbol-function 'require)))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (if (eq feature 'agent-shell)
                     (progn
                       (setq agent-shell-requested t)
                       nil)
                   (funcall original-require feature filename noerror)))))
      (riven/test-with-quiet-keybinding-diagnostics
       (riven/keybindings-config)))
    (should-not agent-shell-requested)))

(ert-deftest riven/ai-code-menu-is-primary-global-entry ()
  "The primary AI coding key invokes `ai-code-menu'."
  (riven/test-with-quiet-keybinding-diagnostics
   (riven/keybindings-config))
  (should (eq (key-binding (kbd "M-*")) #'ai-code-menu)))

(ert-deftest riven/agent-bindings-do-not-depend-on-loaded-package ()
  "Direct agent keys remain configured before agent-shell is loaded."
  (riven/test-with-quiet-keybinding-diagnostics
   (riven/keybindings-config))
  (should (eq (key-binding (kbd "C-c = =")) #'agent-shell))
  (should (eq (key-binding (kbd "C-c = s")) #'agent-shell-setup)))

(defun riven/test-find-use-package-form (relative-path package)
  "Find the `use-package' form for PACKAGE in RELATIVE-PATH."
  (with-temp-buffer
    (insert-file-contents (expand-file-name relative-path riven/test-repo-root))
    (goto-char (point-min))
    (catch 'found
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (consp form)
                         (eq (car form) 'use-package)
                         (eq (cadr form) package))
                (throw 'found form))))
        (end-of-file nil)))))

(ert-deftest riven/agent-shell-bootstrap-dependencies-are-deferred ()
  "Startup declarations do not eagerly load agent transport dependencies."
  (dolist (package '(acp shell-maker))
    (let ((form (riven/test-find-use-package-form
                 "lisp/ai/agent-shell/init-agent-shell-core.el" package)))
      (should form)
      (should (eq (cadr (memq :defer form)) t)))))

(ert-deftest riven/agent-shell-has-no-obsolete-general-keybinding-coupling ()
  "Agent bootstrap does not retain the retired general.el definer."
  (let ((text (riven/test-read-repo-file
               "lisp/ai/agent-shell/init-agent-shell-core.el")))
    (should-not (string-match-p "agent-shell-leader-def" text))
    (should-not (string-match-p ":after general" text))))

(ert-deftest riven/agent-shell-dispatch-is-retired ()
  "The superseded global dispatcher is no longer defined."
  (should-not
   (string-match-p "(defun riven/agent-shell-dispatch"
                   (riven/test-read-repo-file
                    "lisp/ai/agent-shell/init-agent-shell-commands.el"))))

(ert-deftest riven/keybinding-compatibility-shims-are-removed ()
  "Unreferenced split keybinding shims are not retained."
  (dolist (path '("lisp/keymaps/init-keybindings-core.el"
                  "lisp/keymaps/init-keybindings-ai.el"
                  "lisp/keymaps/init-keybindings-org.el"
                  "lisp/keymaps/init-keybindings-session.el"))
    (should-not (file-exists-p (expand-file-name path riven/test-repo-root)))))

(provide 'test-focused-config-simplification)
;;; test-focused-config-simplification.el ends here
