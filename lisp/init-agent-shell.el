;;; Package -- Summary: -*- coding: utf-8; lexical-binding: t -*-
;;; init-agent-shell.el --- config agent-shell

;;; Commentary:
;;; Agent Shell 配置：包括 agent-shell 包和所有相关功能
;;; 注意：Emacs 会在首次使用时提示自动安装缺失的工具
;;; - Claude Code: 需要 Homebrew (macOS)
;;; - Cursor ACP: 需要 npm
;;; - OpenAI Codex: 需要配置 OpenAI API key

;;; Code:

;; ============================================================
;; Agent Shell
;; ============================================================

(use-package agent-shell
  :vc (:url "https://github.com/xenodium/agent-shell" :branch "main")
  :init
  (general-create-definer agent-shell-leader-def
    :prefix "C-c =")
  
  :commands (agent-shell
             agent-shell-new-shell
             agent-shell-toggle
             agent-shell-send-region
             agent-shell-send-current-file
             agent-shell-send-screenshot
             agent-shell-clear-buffer
             agent-shell-search-history
             agent-shell-help-menu
             agent-shell-interrupt
             riven/start-claude-code
             riven/start-open-code
             riven/start-cursor-acp
             riven/agent-shell-diagnose
             riven/install-claude-code
             riven/install-cursor-agent-acp))

;; ============================================================
;; Agent Shell 配置
;; ============================================================

(with-eval-after-load 'agent-shell
  (require 'transient)

  ;; ============================================================
  ;; Agent Shell 辅助函数
  ;; ============================================================

  (defun riven/agent-executable-exists-p (executable)
    "检查 EXECUTABLE 是否在 PATH 中可用."
    (executable-find executable))

  ;; ============================================================
  ;; Agent Shell 安装函数
  ;; ============================================================

  (defun riven/install-cursor-agent-acp ()
    "自动安装 cursor-agent-acp 通过 npm."
    (interactive)
    (if (not (executable-find "npm"))
        (message "错误: npm 未安装。请先安装 Node.js 和 npm。")
      (when (yes-or-no-p "是否要安装 cursor-agent-acp (需要 npm)? ")
        (message "正在安装 cursor-agent-acp...")
        (let ((result (shell-command "npm install -g @blowmage/cursor-agent-acp")))
          (if (= result 0)
              (progn
                (message "✓ cursor-agent-acp 安装成功!")
                ;; 刷新 exec-path
                (setq exec-path (append exec-path (list (expand-file-name "~/.nvm/versions/node/*/bin"))))
                t)
            (message "✗ cursor-agent-acp 安装失败。请手动运行: npm install -g @blowmage/cursor-agent-acp")
            nil)))))

  (defun riven/install-claude-code ()
    "自动安装 Claude Code 通过 Homebrew (仅 macOS)."
    (interactive)
    (cond
     ((not (eq system-type 'darwin))
      (message "Claude Code 自动安装目前仅支持 macOS。请手动从 https://github.com/anthropics/claude-code 安装。"))
     ((not (executable-find "brew"))
      (message "错误: Homebrew 未安装。请先安装 Homebrew 或手动安装 Claude Code。"))
     (t
      (when (yes-or-no-p "是否要安装 Claude Code (需要 Homebrew)? ")
        (message "正在安装 Claude Code...")
        (let ((result (shell-command "brew install --cask claude-code")))
          (if (= result 0)
              (progn
                (message "✓ Claude Code 安装成功!")
                t)
            (message "✗ Claude Code 安装失败。请手动运行: brew install --cask claude-code")
            nil))))))

  (defun riven/install-opencode ()
    "自动安装 Open Code 通过 Homebrew (仅 macOS)"
    (interactive)
    (cond
     ((not (eq system-type 'darwin))
      (message "Open Code 自动安装目前仅支持 macOS。请手动从 https://opencode.ai/ 安装。"))
     ((not (executable-find "brew"))
      (message "错误: Homebrew 未安装。请先安装 Homebrew 或手动安装 Open Code。"))
     (t
      (when (yes-or-no-p "是否要安装 Open Code (需要 Homebrew)? ")
        (message "正在安装 Open Code...")
        (let ((result (shell-command "brew install anomalyco/tap/opencode")))
          (if (= result 0)
              (progn
                (message "✓ Open Code 安装成功!")
                t)
            (message "✗ Open Code 安装失败。请手动运行: brew install anomalyco/tap/opencode")
            nil))))))

  (defun riven/prompt-install-agent (agent-name install-function)
    "提示用户安装 AGENT-NAME，使用 INSTALL-FUNCTION 进行安装."
    (when (yes-or-no-p (format "%s 未安装。是否现在安装? " agent-name))
      (funcall install-function)))

  ;; ============================================================
  ;; Agent Shell 快速启动函数
  ;; ============================================================

  (defun riven/start-claude-code ()
    "快速启动 Claude Code agent，如果未安装则提示安装."
    (interactive)
    (cond
     ((not (fboundp 'agent-shell-anthropic-start-claude-code))
      (message "Claude Code not available in agent-shell. Please update agent-shell package."))
     ((not (riven/agent-executable-exists-p "claude"))
      (if (riven/prompt-install-agent "Claude Code" 'riven/install-claude-code)
          ;; 安装成功后重试启动
          (when (riven/agent-executable-exists-p "claude")
            (agent-shell-anthropic-start-claude-code))
        (message "已取消 Claude Code 安装。")))
     (t
      (agent-shell-anthropic-start-claude-code))))

  (defun riven/start-open-code ()
    "快速启动 Open Code agent，如果未安装则提示安装."
    (interactive)
    (cond
     ((not (fboundp 'agent-shell-opencode-start-agent))
      (message "Open Code not available in agent-shell. Please update agent-shell package."))
     ((not (riven/agent-executable-exists-p "opencode"))
      (if (riven/prompt-install-agent "Opencode" 'riven/install-opencode)
          ;; 安装成功后重试启动
          (when (riven/agent-executable-exists-p "opencode")
            (agent-shell-opencode-start-agent))
        (message "已取消 Open Code 安装。")))
     (t
      (agent-shell-opencode-start-agent))))

  (defun riven/start-cursor-acp ()
    "快速启动 Cursor ACP agent，如果未安装则提示安装."
    (interactive)
    (cond
     ((not (fboundp 'agent-shell-cursor-start-agent))
      (message "Cursor ACP not available in agent-shell. Please update agent-shell package."))
     ((not (riven/agent-executable-exists-p "cursor-agent-acp"))
      (if (riven/prompt-install-agent "Cursor ACP" 'riven/install-cursor-agent-acp)
          ;; 安装成功后重试启动
          (when (riven/agent-executable-exists-p "cursor-agent-acp")
            (agent-shell-cursor-start-agent))
        (message "已取消 Cursor ACP 安装。")))
     (t
      (agent-shell-cursor-start-agent))))

  ;; ============================================================
  ;; Agent Shell 诊断函数
  ;; ============================================================

  (defun riven/agent-shell-diagnose ()
    "诊断并显示所有 agent 的可用性状态."
    (interactive)
    (let ((results '())
          (need-claude nil)
          (need-cursor nil))
      ;; 检查 agent-shell 功能
      (push (format "agent-shell package: %s"
                    (if (featurep 'agent-shell) "✓ 已加载" "✗ 未加载"))
            results)
      (push "" results)

      ;; 检查 Claude Code
      (push "=== Claude Code ===" results)
      (push (format "Function: %s"
                    (if (fboundp 'agent-shell-anthropic-start-claude-code) "✓ 可用" "✗ 不可用"))
            results)
      (let ((has-claude (riven/agent-executable-exists-p "claude")))
        (push (format "Executable: %s"
                      (if has-claude "✓ 已安装" "✗ 未安装"))
              results)
        (setq need-claude (not has-claude)))
      (push "" results)

      ;; 检查 Cursor ACP
      (push "=== Cursor ACP ===" results)
      (push (format "Function: %s"
                    (if (fboundp 'agent-shell-cursor-start-agent) "✓ 可用" "✗ 不可用"))
            results)
      (let ((has-cursor (riven/agent-executable-exists-p "cursor-agent-acp")))
        (push (format "Executable: %s"
                      (if has-cursor "✓ 已安装" "✗ 未安装"))
              results)
        (setq need-cursor (not has-cursor)))
      (push "" results)

      ;; 显示结果
      (let ((buffer (get-buffer-create "*Agent Shell Diagnosis*")))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "=== Agent Shell 诊断报告 ===\n\n")
            (dolist (result (reverse results))
              (insert result "\n"))
            (insert "\n提示: ✓ = 可用, ✗ = 不可用\n\n")

            ;; 添加安装按钮
            (when (or need-claude need-cursor)
              (insert "=== 快速安装 ===\n\n")
              (when need-claude
                (insert-button "安装 Claude Code"
                               'action (lambda (_)
                                         (riven/install-claude-code)
                                         (riven/agent-shell-diagnose))
                               'follow-link t)
                (insert " (需要 Homebrew, 仅 macOS)\n"))
              (when need-cursor
                (insert-button "安装 Cursor ACP"
                               'action (lambda (_)
                                         (riven/install-cursor-agent-acp)
                                         (riven/agent-shell-diagnose))
                               'follow-link t)
                (insert " (需要 npm)\n"))
              (insert "\n"))

            ;; 手动安装说明
            (insert "=== 手动安装命令 ===\n\n")
            (insert "Claude Code (macOS):  brew install anthropics/claude/claude\n")
            (insert "Cursor ACP:           npm install -g @blowmage/cursor-agent-acp\n")
            (insert "OpenAI API Key:       export OPENAI_API_KEY='your-key'\n")
            (goto-char (point-min)))
          (special-mode)
          (local-set-key (kbd "r") 'riven/agent-shell-diagnose))
        (display-buffer buffer))))

  ;; ============================================================
  ;; Agent Shell 发送函数
  ;; ============================================================

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

  ;; ============================================================
  ;; Agent Shell 操作函数
  ;; ============================================================

  (defun riven/agent-shell-undo-last-edit ()
    "Undo last edit by sending agent-specific undo command.
OpenCode uses /undo, Claude Code uses /rewind."
    (interactive)
    (let* ((config (map-elt agent-shell--state :agent-config))
           (agent-name (or (map-elt config :mode-line-name)
                          (map-elt config :buffer-name) "Unknown")))
      (cond
       ((string-match-p "opencode" agent-name :ignore-case)
        (agent-shell-insert :text "/undo" :submit t))
       ((string-match-p "claude" agent-name :ignore-case)
        (agent-shell-insert :text "/rewind" :submit t))
       ((string-match-p "cursor" agent-name :ignore-case)
        (agent-shell-insert :text "/undo" :submit t))
       (t
        (message "Unknown agent type: %s. Try /undo or /rewind" agent-name)))))

  (defun riven/agent-shell-compact-session ()
    "Compact session by sending agent-specific compact command.
All agents use /compact."
    (interactive)
    (let* ((config (map-elt agent-shell--state :agent-config))
           (agent-name (or (map-elt config :mode-line-name)
                          (map-elt config :buffer-name) "Unknown")))
      (cond
       ((string-match-p "opencode" agent-name :ignore-case)
        (agent-shell-insert :text "/compact" :submit t))
       ((string-match-p "claude" agent-name :ignore-case)
        (agent-shell-insert :text "/compact" :submit t))
       ((string-match-p "cursor" agent-name :ignore-case)
        (agent-shell-insert :text "/compact" :submit t))
       (t
        (message "Unknown agent type: %s" agent-name)))))

  (defun riven/agent-shell-review-changes ()
    "Show modified files in magit for review with stage/unstage support."
    (interactive)
    (call-interactively 'magit-status))

  ;; ============================================================
  ;; Agent Shell 代码操作函数
  ;; ============================================================

  (defun riven/agent-shell-explain-code ()
    "Explain selected code with detailed prompt, auto submit."
    (interactive)
    (if (use-region-p)
        (agent-shell-insert
         :text (format "Please use Chinese to explain this code in detail:\n1. What does this code do?\n2. How does it work?\n3. Any potential issues or improvements?\n\n```\n%s\n```"
                       (buffer-substring-no-properties (region-beginning) (region-end)))
         :submit t)
      (message "请选中要解释的代码")))

  (defun riven/agent-shell-refactor-code ()
    "Refactor selected code with detailed prompt, auto submit."
    (interactive)
    (if (use-region-p)
        (agent-shell-insert
         :text (format "Please refactor this code:\n1. Improve readability and maintainability\n2. Follow best practices\n3. Add comments where needed\n\nProvide the refactored code with explanations.\n\n```\n%s\n```"
                       (buffer-substring-no-properties (region-beginning) (region-end)))
         :submit t)
      (message "请选中要重构的代码")))

  (defun riven/agent-shell-add-comments ()
    "Add standard comments to selected code, auto submit."
    (interactive)
    (if (use-region-p)
        (agent-shell-insert
         :text (format "Please add appropriate comments to this code following language best practices:\n1. Add docstring for functions/classes\n2. Explain complex logic with inline comments\n3. Use clear and concise language\n\n```\n%s\n```"
                       (buffer-substring-no-properties (region-beginning) (region-end)))
         :submit t)
      (message "请选中要添加注释的代码")))

  (defun riven/agent-shell-fix-errors ()
    "Fix flymake errors with detailed prompt, auto submit."
    (interactive)
    (require 'flymake)
    (let ((errors (flymake-diagnostics-at-point (point-min) (point-max))))
      (if errors
          (let* ((diag (car errors))
                 (line (flymake-diagnostic-line diag))
                 (text (flymake-diagnostic-text diag))
                 (buffer-file (buffer-file-name))
                 (file-content (when buffer-file
                                (with-temp-buffer
                                  (insert-file-contents buffer-file)
                                  (goto-char (point-min))
                                  (forward-line (1- line))
                                  (let ((start (point)))
                                    (forward-line 1)
                                    (buffer-substring start (point)))))))
            (agent-shell-insert
             :text (format "Please fix this flymake error at line %d:\n1. Identify the root cause\n2. Provide the corrected code\n3. Explain what was wrong and how you fixed it\n\nContext:\n```\n%s\n```\n\nError: %s"
                           line file-content text)
             :submit t))
        (message "当前缓冲区没有flymake错误"))))

  ;; ============================================================
  ;; Agent Shell Transient 菜单
  ;; ============================================================

  (defun agent-shell-buffer-exists-p ()
    "检查是否存在任何 agent-shell buffer."
    (cl-some (lambda (buf)
               (with-current-buffer buf
                 (derived-mode-p 'agent-shell-mode)))
             (buffer-list)))

  (transient-define-prefix agent-shell-transient ()
    "Agent Shell Commands"

    [:description
     (lambda () "🤖 Agent Shell Commands")

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

    [:if agent-shell-buffer-exists-p
     ["📤 Send"
      ("sr" "Send region/file" riven/agent-shell-send-dwim-or-file)
      ("sd" "Send directory" riven/agent-shell-send-directory)
      ("sf" "Send file" agent-shell-send-current-file)
      ("ss" "Screenshot" agent-shell-send-screenshot)]

     ["🔧 Code & Review"
      ("v" "Review changes" riven/agent-shell-review-changes)
      ("u" "Undo last edit" riven/agent-shell-undo-last-edit)
      ("f" "Fix error" riven/agent-shell-fix-errors)
      ("e" "Explain code" riven/agent-shell-explain-code)
      ("r" "Refactor code" riven/agent-shell-refactor-code)
      ("/" "Add comments" riven/agent-shell-add-comments)]

     ["⚡ Control"
      ("I" "Interrupt" agent-shell-interrupt)
      ("C" "Clear buffer" agent-shell-clear-buffer)
      ("H" "Search history" agent-shell-search-history)
      ("K" "Compact session" riven/agent-shell-compact-session)]]

    [["❓ Help & Quit"
      ("?" "Help" agent-shell-help-menu)
      ("q" "Quit" transient-quit-one)]])

  (message "Agent-shell configuration loaded"))

;; ============================================================
;; 结束
;; ============================================================

(provide 'init-agent-shell)

;;; init-agent-shell.el ends here
