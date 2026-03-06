;;; init-agent-shell.el --- Agent shell integration -*- lexical-binding: t; -*-

(eval-and-compile
  (let ((current-file (or load-file-name byte-compile-current-file buffer-file-name)))
    (when current-file
      (add-to-list 'load-path (file-name-directory current-file)))))

(require 'init-agent-shell-core)
(require 'init-agent-shell-install)
(require 'init-agent-shell-commands)

(with-eval-after-load 'agent-shell
  (require 'init-agent-shell-ui)
  (message "Agent-shell configuration loaded"))

(provide 'init-agent-shell)
