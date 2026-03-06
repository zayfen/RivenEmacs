;;; init-agent-shell-commands.el --- Agent shell commands -*- lexical-binding: t; -*-

(eval-and-compile
  (let ((current-file (or load-file-name byte-compile-current-file buffer-file-name)))
    (when current-file
      (add-to-list 'load-path (file-name-directory current-file)))))

(require 'init-agent-shell-core)
(require 'init-agent-shell-install)

(defun riven/start-claude-code ()
  "Start Claude Code, prompting install when unavailable."
  (interactive)
  (cond
   ((not (fboundp 'agent-shell-anthropic-start-claude-code))
    (message "Claude Code not available in agent-shell. Please update agent-shell package."))
   ((not (riven/agent-executable-exists-p "claude"))
    (if (riven/prompt-install-agent "Claude Code" #'riven/install-claude-code)
        (when (riven/agent-executable-exists-p "claude")
          (agent-shell-anthropic-start-claude-code))
      (message "已取消 Claude Code 安装。")))
   (t (agent-shell-anthropic-start-claude-code))))

(defun riven/start-open-code ()
  "Start Open Code, prompting install when unavailable."
  (interactive)
  (cond
   ((not (fboundp 'agent-shell-opencode-start-agent))
    (message "Open Code not available in agent-shell. Please update agent-shell package."))
   ((not (riven/agent-executable-exists-p "opencode"))
    (if (riven/prompt-install-agent "Opencode" #'riven/install-opencode)
        (when (riven/agent-executable-exists-p "opencode")
          (agent-shell-opencode-start-agent))
      (message "已取消 Open Code 安装。")))
   (t (agent-shell-opencode-start-agent))))

(defun riven/start-cursor-acp ()
  "Start Cursor ACP, prompting install when unavailable."
  (interactive)
  (cond
   ((not (fboundp 'agent-shell-cursor-start-agent))
    (message "Cursor ACP not available in agent-shell. Please update agent-shell package."))
   ((not (riven/agent-executable-exists-p "cursor-agent-acp"))
    (if (riven/prompt-install-agent "Cursor ACP" #'riven/install-cursor-agent-acp)
        (when (riven/agent-executable-exists-p "cursor-agent-acp")
          (agent-shell-cursor-start-agent))
      (message "已取消 Cursor ACP 安装。")))
   (t (agent-shell-cursor-start-agent))))

(defun riven/agent-shell-send-dwim-or-file ()
  "Send region if active, otherwise send current file."
  (interactive)
  (if (use-region-p)
      (agent-shell-send-region)
    (agent-shell-send-current-file)))

(defun riven/agent-shell-send-directory ()
  "Send current directory to agent shell."
  (interactive)
  (agent-shell-insert :text (format "Current directory: `%s`" default-directory)))

(defun riven/agent-shell-undo-last-edit ()
  "Send agent-specific undo command."
  (interactive)
  (let ((agent-name (riven/agent-shell-current-agent-name)))
    (cond
     ((string-match-p "opencode" agent-name :ignore-case)
      (agent-shell-insert :text "/undo" :submit t))
     ((string-match-p "claude" agent-name :ignore-case)
      (agent-shell-insert :text "/rewind" :submit t))
     ((string-match-p "cursor" agent-name :ignore-case)
      (agent-shell-insert :text "/undo" :submit t))
     (t (message "Unknown agent type: %s. Try /undo or /rewind" agent-name)))))

(defun riven/agent-shell-compact-session ()
  "Send compact command for current agent session."
  (interactive)
  (let ((agent-name (riven/agent-shell-current-agent-name)))
    (if (or (string-match-p "opencode" agent-name :ignore-case)
            (string-match-p "claude" agent-name :ignore-case)
            (string-match-p "cursor" agent-name :ignore-case))
        (agent-shell-insert :text "/compact" :submit t)
      (message "Unknown agent type: %s" agent-name))))

(defun riven/agent-shell-review-changes ()
  "Open Magit status to review current changes."
  (interactive)
  (call-interactively #'magit-status))

(defun riven/agent-shell-dispatch ()
  "Start agent-shell or show transient when session exists."
  (interactive)
  (if (and (fboundp 'agent-shell-buffers)
           (agent-shell-buffers))
      (when (fboundp 'agent-shell-transient)
        (agent-shell-transient))
    (call-interactively #'agent-shell)))

(provide 'init-agent-shell-commands)
