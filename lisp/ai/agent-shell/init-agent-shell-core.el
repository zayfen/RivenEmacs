;;; init-agent-shell-core.el --- Agent shell package bootstrap -*- lexical-binding: t; -*-

(require 'init-prompt-template nil t)

(use-package acp
  :vc (:url "https://github.com/xenodium/acp.el" :branch "main"))

(use-package shell-maker
  :vc (:url "https://github.com/xenodium/shell-maker" :branch "main"))

(use-package agent-shell
  :vc (:url "https://github.com/xenodium/agent-shell" :branch "main")
  :after general
  :init
  (general-create-definer agent-shell-leader-def
    :prefix "C-c =")
  :commands (agent-shell agent-shell-new-shell agent-shell-toggle
             agent-shell-send-region agent-shell-send-current-file
             agent-shell-send-screenshot agent-shell-clear-buffer
             agent-shell-search-history agent-shell-help-menu
             agent-shell-interrupt riven/start-claude-code
             riven/start-open-code riven/start-cursor-acp
             riven/agent-shell-diagnose riven/install-claude-code
             riven/install-cursor-agent-acp))

(defun riven/agent-executable-exists-p (executable)
  "Return non-nil when EXECUTABLE exists in PATH."
  (executable-find executable))

(defun riven/agent-shell-current-agent-name ()
  "Return current agent-shell mode-line name for command routing."
  (let* ((config (map-elt agent-shell--state :agent-config)))
    (or (map-elt config :mode-line-name)
        (map-elt config :buffer-name)
        "Unknown")))

(provide 'init-agent-shell-core)
