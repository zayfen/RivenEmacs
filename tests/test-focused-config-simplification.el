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

(defun riven/test-run-emacs-batch (&rest args)
  "Run Emacs in batch mode with ARGS and return captured output."
  (with-temp-buffer
    (let ((status (apply #'call-process
                         (car command-line-args)
                         nil
                         (current-buffer)
                         nil
                         args)))
      (cons status (buffer-string)))))

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

(ert-deftest riven/early-init-initializes-package-system ()
  "Startup initializes package.el so installed ELPA packages are discoverable."
  (let* ((early-init (expand-file-name "early-init.el" riven/test-repo-root))
         (result (riven/test-run-emacs-batch
                  "-Q" "--batch"
                  "--eval" (format "(setq user-emacs-directory %S)" riven/test-repo-root)
                  "-l" early-init
                  "--eval" "(message \"package-init=%S general=%S vterm=%S\" package--initialized (locate-library \"general\") (locate-library \"vterm\"))"))
         (status (car result))
         (output (cdr result)))
    (should (= status 0))
    (should (string-match-p "package-init=t" output))
    (should (string-match-p "general\\.el" output))
    (should (string-match-p "vterm\\.el" output))))

(ert-deftest riven/direct-init-load-initializes-package-system ()
  "`emacs --batch -l init.el` should initialize package.el for CI checks."
  (let* ((init-file (expand-file-name "init.el" riven/test-repo-root))
         (result (riven/test-run-emacs-batch
                  "--batch"
                  "-l" init-file
                  "--eval" "(message \"direct-package-init=%S general=%S vterm=%S\" (bound-and-true-p package--initialized) (locate-library \"general\") (locate-library \"vterm\"))"))
         (status (car result))
         (output (cdr result)))
    (should (= status 0))
    (should (string-match-p "direct-package-init=t" output))
    (should (string-match-p "general\\.el" output))
    (should (string-match-p "vterm\\.el" output))))

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

(ert-deftest riven/general-startup-dependency-is-retired ()
  "Declarative keybindings no longer need the general.el startup module."
  (should-not
   (string-match-p "(require 'init-general)"
                   (riven/test-read-repo-file "init.el")))
  (should-not
   (string-match-p "(leader-def"
                   (riven/test-read-repo-file "lisp/ide/init-debugger.el")))
  (should-not
   (file-exists-p
    (expand-file-name "lisp/ui/init-general.el" riven/test-repo-root))))

(ert-deftest riven/command-packages-use-lazy-declarations ()
  "Rarely used command packages should not be demanded during startup."
  (let ((iedit (riven/test-find-use-package-form "lisp/editor/init-editor.el" 'iedit))
        (sudo-edit (riven/test-find-use-package-form "lisp/editor/init-editor.el" 'sudo-edit))
        (vterm (riven/test-find-use-package-form "lisp/tools/init-terminal.el" 'vterm)))
    (should iedit)
    (should-not (memq :demand iedit))
    (should (memq :commands iedit))
    (should sudo-edit)
    (should (memq :commands sudo-edit))
    (should vterm)
    (should (memq :commands vterm))))

(ert-deftest riven/completion-stack-has-focused-modules ()
  "Completion configuration is split by responsibility."
  (dolist (path '("lisp/completion/init-minibuffer.el"
                  "lisp/completion/init-completion-ui.el"
                  "lisp/completion/init-consult.el"))
    (should (file-exists-p (expand-file-name path riven/test-repo-root))))
  (let ((init-text (riven/test-read-repo-file "init.el"))
        (consult-text (riven/test-read-repo-file "lisp/completion/init-consult.el")))
    (should (string-match-p "(require 'init-minibuffer)" init-text))
    (should (string-match-p "(require 'init-completion-ui)" init-text))
    (should (string-match-p "(provide 'init-consult)" consult-text))
    (should (string-match-p "(require 'init-minibuffer)" consult-text))
    (should (string-match-p "(require 'init-completion-ui)" consult-text))))

(ert-deftest riven/agent-shell-aggregator-stays-lightweight ()
  "The agent-shell aggregator should not eagerly load command/install modules."
  (let ((text (riven/test-read-repo-file
               "lisp/ai/agent-shell/init-agent-shell.el")))
    (should-not (string-match-p "(require 'init-agent-shell-install)" text))
    (should-not (string-match-p "(require 'init-agent-shell-commands)" text))
    (should (string-match-p "(autoload 'agent-shell-setup" text))
    (should (string-match-p "(autoload 'riven/start-claude-code" text))))

(ert-deftest riven/keybinding-compatibility-shims-are-removed ()
  "Unreferenced split keybinding shims are not retained."
  (dolist (path '("lisp/keymaps/init-keybindings-core.el"
                  "lisp/keymaps/init-keybindings-ai.el"
                  "lisp/keymaps/init-keybindings-org.el"
                  "lisp/keymaps/init-keybindings-session.el"))
    (should-not (file-exists-p (expand-file-name path riven/test-repo-root)))))

(provide 'test-focused-config-simplification)
;;; test-focused-config-simplification.el ends here
