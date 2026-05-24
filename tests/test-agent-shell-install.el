;;; test-agent-shell-install.el --- Agent shell installer tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'package)

(defconst riven/test-agent-shell-repo-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root used by agent shell installer tests.")

(defun riven/test-agent-shell-read-repo-file (relative-path)
  "Return contents of RELATIVE-PATH under `riven/test-agent-shell-repo-root'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name relative-path riven/test-agent-shell-repo-root))
    (buffer-string)))

(setq package-user-dir (expand-file-name "elpa" riven/test-agent-shell-repo-root))
(unless package--initialized
  (package-initialize))

(require 'init-use-package)
(require 'init-agent-shell-install)
(require 'init-agent-shell-commands)

(ert-deftest riven/agent-shell-setup-upgrades-required-npm-agents ()
  "`agent-shell-setup' installs the npm agents required by agent-shell."
  (let (commands)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program)
                 (and (equal program "npm") "/usr/local/bin/npm")))
              ((symbol-function 'shell-command)
               (lambda (command)
                 (push command commands)
                 0))
              ((symbol-function 'riven/agent-shell-upgrade-vc-packages)
               (lambda () t)))
      (should (agent-shell-setup))
      (dolist (command '("npm install -g @blowmage/cursor-agent-acp@latest"
                         "npm install -g @openai/codex@latest"
                         "npm install -g @zed-industries/codex-acp@latest"
                         "npm install -g @zed-industries/claude-agent-acp@latest"))
        (should (member command commands))))))

(ert-deftest riven/agent-shell-setup-skips-npm-agents-without-npm ()
  "`agent-shell-setup' reports failure and avoids shelling out when npm is unavailable."
  (let (commands)
    (cl-letf (((symbol-function 'executable-find) #'ignore)
              ((symbol-function 'shell-command)
               (lambda (command)
                 (push command commands)
                 0))
              ((symbol-function 'riven/agent-shell-upgrade-vc-packages)
               (lambda () t)))
      (should-not (agent-shell-setup))
      (should-not commands))))

(ert-deftest riven/claude-code-start-checks-claude-acp-binary ()
  "`riven/start-claude-code' checks the ACP adapter binary before starting."
  (let (checked started)
    (cl-letf (((symbol-function 'agent-shell-anthropic-start-claude-code)
               (lambda ()
                 (setq started t)))
              ((symbol-function 'riven/agent-executable-exists-p)
               (lambda (program)
                 (push program checked)
                 (equal program "claude-agent-acp")))
              ((symbol-function 'riven/prompt-install-agent)
               (lambda (&rest _args)
                 (ert-fail "install prompt should not be used when ACP binary exists"))))
      (riven/start-claude-code)
      (should started)
      (should (equal checked '("claude-agent-acp"))))))

(ert-deftest riven/agent-shell-install-commands-are-interactive ()
  "New agent installer commands are available as interactive commands."
  (dolist (command '(riven/install-codex
                     riven/install-codex-acp
                     riven/install-claude-agent-acp
                     riven/install-cursor-agent-acp))
    (should (fboundp command))
    (should (commandp command))))

(ert-deftest riven/dependency-script-installs-agent-shell-npm-agents ()
  "`scripts/riven-deps.sh' keeps its AI npm dependencies aligned with setup."
  (let ((script (riven/test-agent-shell-read-repo-file "scripts/riven-deps.sh")))
    (dolist (package '("@blowmage/cursor-agent-acp"
                       "@openai/codex"
                       "@zed-industries/codex-acp"
                       "@zed-industries/claude-agent-acp"))
      (should (string-match-p (regexp-quote (format "npm_global_install %s" package))
                              script)))))

(provide 'test-agent-shell-install)
;;; test-agent-shell-install.el ends here
