;;; init-ai-code.el --- AI Code Interface configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Unified AI coding workflow for CLI-based coding agents.

;;; Code:

(defcustom rivenEmacs-ai-code-backend 'codex
  "Preferred backend for `ai-code'."
  :type '(choice (const :tag "OpenAI Codex CLI" codex)
                 (const :tag "Claude Code" claude-code)
                 (const :tag "Opencode" opencode)
                 (const :tag "Aider" aider))
  :group 'rivenEmacs)

(defun riven/ai-code-configure-backend ()
  "Configure `ai-code' with `rivenEmacs-ai-code-backend'."
  (if (fboundp 'ai-code-set-backend)
      (condition-case err
          (ai-code-set-backend rivenEmacs-ai-code-backend)
        (error
         (message "[ai-code] Failed to set backend %s: %s"
                  rivenEmacs-ai-code-backend
                  (error-message-string err))))
    (message "[ai-code] ai-code-set-backend is unavailable.")))

(use-package ai-code
  :vc (:url "https://github.com/tninja/ai-code-interface.el" :branch "main")
  :commands (ai-code-menu ai-code-cli-start ai-code-cli-resume
             ai-code-cli-switch-to-buffer ai-code-send-command
             ai-code-code-change ai-code-ask-question)
  :init
  (autoload 'ai-code-set-backend "ai-code-backends" nil t)
  :custom
  (ai-code-backends-infra-terminal-backend 'ghostel)
  (ai-code-menu-layout 'two-columns)
  (ai-code-auto-test-type 'ask-me)
  :config
  (riven/ai-code-configure-backend)
  (with-eval-after-load 'magit
    (when (fboundp 'ai-code-magit-setup-transients)
      (ai-code-magit-setup-transients))))

(provide 'init-ai-code)

;;; init-ai-code.el ends here
