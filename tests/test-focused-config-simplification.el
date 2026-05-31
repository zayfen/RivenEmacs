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

(ert-deftest riven/keybindings-agent-spec-uses-ai-code-only ()
  "Dedicated agent keys should route through ai-code, not agent-shell."
  (should-not
   (cl-some (lambda (entry)
              (memq (cadr entry) '(agent-shell agent-shell-setup)))
            riven/keybindings-agent-spec)))

(ert-deftest riven/ai-code-menu-is-primary-global-entry ()
  "The primary AI coding key invokes `ai-code-menu'."
  (riven/test-with-quiet-keybinding-diagnostics
   (riven/keybindings-config))
  (should (eq (key-binding (kbd "M-*")) #'ai-code-menu)))

(ert-deftest riven/agent-bindings-use-ai-code-entrypoints ()
  "Dedicated agent keys expose ai-code entrypoints only."
  (riven/test-with-quiet-keybinding-diagnostics
   (riven/keybindings-config))
  (should (eq (key-binding (kbd "C-c = =")) #'ai-code-menu))
  (should (eq (key-binding (kbd "C-c = 1")) #'ai-code-claude-code))
  (should (eq (key-binding (kbd "C-c = 2")) #'ai-code-opencode))
  (should (eq (key-binding (kbd "C-c = 3")) #'ai-code-cursor-cli))
  (should-not (key-binding (kbd "C-c = s"))))

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

(ert-deftest riven/c-x-d-opens-dirvish-on-first-use ()
  "`C-x d' should open a Dirvish-backed Dired session immediately."
  (let* ((init-file (expand-file-name "init.el" riven/test-repo-root))
         (result (riven/test-run-emacs-batch
                  "--batch"
                  "-l" init-file
                  "--eval"
                  "(let ((binding (key-binding (kbd \"C-x d\"))))
                     (when (eq binding 'dirvish)
                       (call-interactively binding))
                     (message \"c-x-d-binding=%S major=%S dirvish-session=%S\"
                              binding
                              major-mode
                              (and (fboundp 'dirvish-curr)
                                   (not (null (dirvish-curr))))))"))
         (status (car result))
         (output (cdr result)))
    (should (= status 0))
    (should (string-match-p "c-x-d-binding=dirvish" output))
    (should (string-match-p "major=dired-mode" output))
    (should (string-match-p "dirvish-session=t" output))))

(ert-deftest riven/dired-prefers-homebrew-gls-outside-exec-path ()
  "Dired should find Homebrew gls even before shell PATH sync runs."
  (skip-unless (file-executable-p "/opt/homebrew/bin/gls"))
  (let* ((init-file (expand-file-name "init.el" riven/test-repo-root))
         (result (riven/test-run-emacs-batch
                  "--batch"
                  "--eval"
                  "(setq exec-path
                         (seq-remove
                          (lambda (dir)
                            (and dir (string-match-p \"/opt/homebrew/bin\" dir)))
                          exec-path))"
                  "-l" init-file
                  "--eval"
                  "(progn
                     (require 'dired)
                     (message \"insert-directory-program=%S dired-listing-switches=%S\"
                              insert-directory-program
                              dired-listing-switches))"))
         (status (car result))
         (output (cdr result)))
    (should (= status 0))
    (should (string-match-p "insert-directory-program=\"/opt/homebrew/bin/gls\"" output))
    (should (string-match-p "--group-directories-first" output))))

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

(ert-deftest riven/agent-shell-startup-module-is-retired ()
  "The active startup path should not require the retired agent-shell module."
  (should-not
   (string-match-p "(require 'init-agent-shell)"
                   (riven/test-read-repo-file "init.el"))))

(ert-deftest riven/agent-shell-integration-files-are-removed ()
  "Retired agent-shell modules should not remain in the active lisp tree."
  (dolist (path '("lisp/ai/agent-shell/init-agent-shell.el"
                  "lisp/ai/agent-shell/init-agent-shell-core.el"
                  "lisp/ai/agent-shell/init-agent-shell-install.el"
                  "lisp/ai/agent-shell/init-agent-shell-commands.el"
                  "lisp/ai/agent-shell/init-agent-shell-ui.el"))
    (should-not (file-exists-p (expand-file-name path riven/test-repo-root)))))

(ert-deftest riven/ai-code-default-backend-is-codex ()
  "ai-code should default directly to the Codex backend."
  (let ((text (riven/test-read-repo-file "lisp/ai/init-ai-code.el")))
    (should (string-match-p "(defcustom rivenEmacs-ai-code-backend 'codex" text))
    (should-not (string-match-p "(const :tag \"agent-shell\" agent-shell)" text))))

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

(ert-deftest riven/keybinding-compatibility-shims-are-removed ()
  "Unreferenced split keybinding shims are not retained."
  (dolist (path '("lisp/keymaps/init-keybindings-core.el"
                  "lisp/keymaps/init-keybindings-ai.el"
                  "lisp/keymaps/init-keybindings-org.el"
                  "lisp/keymaps/init-keybindings-session.el"))
    (should-not (file-exists-p (expand-file-name path riven/test-repo-root)))))

(provide 'test-focused-config-simplification)
;;; test-focused-config-simplification.el ends here
