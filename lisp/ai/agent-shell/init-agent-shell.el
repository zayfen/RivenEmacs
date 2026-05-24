;;; init-agent-shell.el --- Agent shell integration -*- lexical-binding: t; -*-

(require 'init-agent-shell-core)

(autoload 'agent-shell-setup "init-agent-shell-install" nil t)
(autoload 'riven/install-cursor-agent-acp "init-agent-shell-install" nil t)
(autoload 'riven/install-codex "init-agent-shell-install" nil t)
(autoload 'riven/install-codex-acp "init-agent-shell-install" nil t)
(autoload 'riven/install-claude-agent-acp "init-agent-shell-install" nil t)
(autoload 'riven/install-claude-code "init-agent-shell-install" nil t)
(autoload 'riven/install-opencode "init-agent-shell-install" nil t)
(autoload 'riven/start-claude-code "init-agent-shell-commands" nil t)
(autoload 'riven/start-open-code "init-agent-shell-commands" nil t)
(autoload 'riven/start-cursor-acp "init-agent-shell-commands" nil t)
(autoload 'riven/agent-shell-send-dwim-or-file "init-agent-shell-commands" nil t)
(autoload 'riven/agent-shell-send-directory "init-agent-shell-commands" nil t)
(autoload 'riven/agent-shell-undo-last-edit "init-agent-shell-commands" nil t)
(autoload 'riven/agent-shell-compact-session "init-agent-shell-commands" nil t)
(autoload 'riven/agent-shell-review-changes "init-agent-shell-commands" nil t)

(with-eval-after-load 'agent-shell
  (require 'init-agent-shell-ui)
  (message "Agent-shell configuration loaded"))

(provide 'init-agent-shell)
