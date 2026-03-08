;;; init-agent-shell.el --- Agent shell integration -*- lexical-binding: t; -*-

(require 'init-agent-shell-core)
(require 'init-agent-shell-install)
(require 'init-agent-shell-commands)

(with-eval-after-load 'agent-shell
  (require 'init-agent-shell-ui)
  (message "Agent-shell configuration loaded"))

(provide 'init-agent-shell)
