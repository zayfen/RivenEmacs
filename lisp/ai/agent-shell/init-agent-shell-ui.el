;;; init-agent-shell-ui.el --- Agent shell UI and diagnosis -*- lexical-binding: t; -*-

(eval-and-compile
  (let ((current-file (or load-file-name byte-compile-current-file buffer-file-name)))
    (when current-file
      (add-to-list 'load-path (file-name-directory current-file)))))

(require 'init-agent-shell-core)
(require 'init-agent-shell-install)
(require 'init-agent-shell-commands)
(require 'transient)

(defun riven/agent-shell-diagnose ()
  "Diagnose and display availability of configured agents."
  (interactive)
  (let ((results '())
        (need-claude nil)
        (need-cursor nil))
    (push (format "agent-shell package: %s"
                  (if (featurep 'agent-shell) "✓ 已加载" "✗ 未加载"))
          results)
    (push "" results)

    (push "=== Claude Code ===" results)
    (push (format "Function: %s"
                  (if (fboundp 'agent-shell-anthropic-start-claude-code) "✓ 可用" "✗ 不可用"))
          results)
    (let ((has-claude (riven/agent-executable-exists-p "claude")))
      (push (format "Executable: %s" (if has-claude "✓ 已安装" "✗ 未安装")) results)
      (setq need-claude (not has-claude)))
    (push "" results)

    (push "=== Cursor ACP ===" results)
    (push (format "Function: %s"
                  (if (fboundp 'agent-shell-cursor-start-agent) "✓ 可用" "✗ 不可用"))
          results)
    (let ((has-cursor (riven/agent-executable-exists-p "cursor-agent-acp")))
      (push (format "Executable: %s" (if has-cursor "✓ 已安装" "✗ 未安装")) results)
      (setq need-cursor (not has-cursor)))
    (push "" results)

    (let ((buffer (get-buffer-create "*Agent Shell Diagnosis*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "=== Agent Shell 诊断报告 ===

")
          (dolist (result (reverse results))
            (insert result "
"))
          (insert "
提示: ✓ = 可用, ✗ = 不可用

")
          (when (or need-claude need-cursor)
            (insert "=== 快速安装 ===

")
            (when need-claude
              (insert-button "安装 Claude Code"
                             'action (lambda (_)
                                       (riven/install-claude-code)
                                       (riven/agent-shell-diagnose))
                             'follow-link t)
              (insert " (需要 Homebrew, 仅 macOS)
"))
            (when need-cursor
              (insert-button "安装 Cursor ACP"
                             'action (lambda (_)
                                       (riven/install-cursor-agent-acp)
                                       (riven/agent-shell-diagnose))
                             'follow-link t)
              (insert " (需要 npm)
"))
            (insert "
"))
          (insert "=== 手动安装命令 ===

")
          (insert "Claude Code (macOS):  brew install anthropics/claude/claude
")
          (insert "Cursor ACP:           npm install -g @blowmage/cursor-agent-acp
")
          (insert "OpenAI API Key:       export OPENAI_API_KEY='your-key'
")
          (goto-char (point-min)))
        (special-mode)
        (local-set-key (kbd "r") #'riven/agent-shell-diagnose))
      (display-buffer buffer))))

(defun riven/agent-shell-explain-code ()
  "Explain selected code with prompt template."
  (interactive)
  (if (use-region-p)
      (agent-shell-insert :text (format riven-prompt-explain-code
                                        (buffer-substring-no-properties (region-beginning) (region-end)))
                          :submit t)
    (message "请选中要解释的代码")))

(defun riven/agent-shell-refactor-code ()
  "Refactor selected code with prompt template."
  (interactive)
  (if (use-region-p)
      (agent-shell-insert :text (format riven-prompt-refactor-code
                                        (buffer-substring-no-properties (region-beginning) (region-end)))
                          :submit t)
    (message "请选中要重构的代码")))

(defun riven/agent-shell-add-comments ()
  "Add comments to selected code with prompt template."
  (interactive)
  (if (use-region-p)
      (agent-shell-insert :text (format riven-prompt-add-comments
                                        (buffer-substring-no-properties (region-beginning) (region-end)))
                          :submit t)
    (message "请选中要添加注释的代码")))

(defun riven/agent-shell-fix-errors ()
  "Fix flymake diagnostics at point with prompt template."
  (interactive)
  (require 'flymake)
  (let ((errors (flymake-diagnostics (point) (point))))
    (message "%s" errors)
    (if errors
        (agent-shell-insert :text (format riven-prompt-fix-errors errors) :submit t)
      (message "当前缓冲区没有flymake错误"))))

(defun riven/agent-shell-buffer-exists-p ()
  "Return non-nil when any agent-shell buffer exists."
  (cl-some (lambda (buf)
             (with-current-buffer buf
               (derived-mode-p 'agent-shell-mode)))
           (buffer-list)))

(transient-define-prefix agent-shell-transient ()
  "Agent Shell command menu."
  [:description (lambda () "Agent Shell Commands")
   ["Start"
    ("1" "Claude Code" riven/start-claude-code)
    ("2" "Open Code" riven/start-open-code)
    ("3" "Cursor ACP" riven/start-cursor-acp)]
   ["Basic"
    ("n" "New shell" agent-shell-new-shell)
    ("N" "Start/reuse" agent-shell)
    ("t" "Toggle" agent-shell-toggle)
    ("d" "Diagnose" riven/agent-shell-diagnose)]
   ["Install"
    ("i1" "Claude Code" riven/install-claude-code)
    ("i2" "Open Code" riven/install-opencode)
    ("i3" "Cursor ACP" riven/install-cursor-agent-acp)]]
  [:if riven/agent-shell-buffer-exists-p
   ["Send"
    ("sr" "Send region/file" riven/agent-shell-send-dwim-or-file)
    ("sd" "Send directory" riven/agent-shell-send-directory)
    ("sf" "Send file" agent-shell-send-current-file)
    ("ss" "Screenshot" agent-shell-send-screenshot)]
   ["Code & Review"
    ("v" "Review changes" riven/agent-shell-review-changes)
    ("u" "Undo last edit" riven/agent-shell-undo-last-edit)
    ("f" "Fix error" riven/agent-shell-fix-errors)
    ("e" "Explain code" riven/agent-shell-explain-code)
    ("r" "Refactor code" riven/agent-shell-refactor-code)
    ("/" "Add comments" riven/agent-shell-add-comments)]
   ["Control"
    ("I" "Interrupt" agent-shell-interrupt)
    ("C" "Clear buffer" agent-shell-clear-buffer)
    ("H" "Search history" agent-shell-search-history)
    ("K" "Compact session" riven/agent-shell-compact-session)]]
  [["Help & Quit"
    ("?" "Help" agent-shell-help-menu)
    ("q" "Quit" transient-quit-one)]])

(provide 'init-agent-shell-ui)
