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
  :ensure t
  :commands (agent-shell
             agent-shell-new-shell
             agent-shell-toggle
             riven/start-claude-code
             riven/start-cursor-acp
             riven/start-openai-codex
             riven/agent-shell-diagnose
             riven/install-claude-code
             riven/install-cursor-agent-acp
             agent-shell-transient))

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
        (let ((result (shell-command "brew install anthropics/claude/claude")))
          (if (= result 0)
              (progn
                (message "✓ Claude Code 安装成功!")
                t)
            (message "✗ Claude Code 安装失败。请手动运行: brew install anthropics/claude/claude")
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
  
  (defun riven/start-openai-codex ()
    "快速启动 OpenAI Codex agent."
    (interactive)
    (cond
     ((not (fboundp 'agent-shell-openai-start-codex))
      (message "OpenAI Codex not available in agent-shell. Please update agent-shell package."))
     ((not (getenv "OPENAI_API_KEY"))
      (message "OpenAI API Key 未设置。请设置环境变量 OPENAI_API_KEY。"))
     (t
      (agent-shell-openai-start-codex))))
  
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
      
      ;; 检查 OpenAI Codex
      (push "=== OpenAI Codex ===" results)
      (push (format "Function: %s"
                    (if (fboundp 'agent-shell-openai-start-codex) "✓ 可用" "✗ 不可用"))
            results)
      (push (format "API Key: %s"
                    (if (getenv "OPENAI_API_KEY") "✓ 已设置" "✗ 未设置"))
            results)
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
  ;; Agent Shell 代码操作辅助函数
  ;; ============================================================
  
  (defun riven/agent-shell-send-current-function ()
    "发送当前函数到 agent shell."
    (interactive)
    (if (derived-mode-p 'prog-mode)
        (let ((func-start (save-excursion (beginning-of-defun) (point)))
              (func-end (save-excursion (end-of-defun) (point))))
          (when (> func-end func-start)
            (agent-shell-send-region func-start func-end)))
      (message "Not in a programming mode")))
  
  (defun riven/agent-shell-send-buffer-context ()
    "发送 buffer 上下文信息到 agent shell."
    (interactive)
    (when buffer-file-name
      (let ((context (format "Context: %s (mode: %s, line: %d)"
                             buffer-file-name
                             major-mode
                             (line-number-at-pos))))
        (agent-shell-send-string context))))
  
  (defun riven/agent-shell-send-project-context ()
    "发送项目上下文信息到 agent shell."
    (interactive)
    (when (project-current)
      (let* ((project-root (project-root (project-current)))
             (project-name (file-name-nondirectory (directory-file-name project-root)))
             (context (format "Project: %s (root: %s)" project-name project-root)))
        (agent-shell-send-string context))))
  
  ;; ============================================================
  ;; Agent Shell 代码操作命令
  ;; ============================================================
  
  (defun riven/agent-shell-code-review ()
    "发送当前函数进行代码审查."
    (interactive)
    (riven/agent-shell-send-current-function)
    (agent-shell-send-string "Please review this code for best practices, potential bugs, and improvements."))
  
  (defun riven/agent-shell-explain-code ()
    "请求 agent 解释当前代码."
    (interactive)
    (riven/agent-shell-send-current-function)
    (agent-shell-send-string "Please explain what this code does in detail."))
  
  (defun riven/agent-shell-refactor-code ()
    "请求 agent 重构当前代码."
    (interactive)
    (riven/agent-shell-send-current-function)
    (agent-shell-send-string "Please refactor this code to improve readability and maintainability."))
  
  (defun riven/agent-shell-add-tests ()
    "请求 agent 为当前代码添加测试."
    (interactive)
    (riven/agent-shell-send-current-function)
    (agent-shell-send-string "Please add comprehensive tests for this code."))
  
  ;; ============================================================
  ;; Agent Shell 项目分析命令
  ;; ============================================================
  
  (defun riven/agent-shell-analyze-project ()
    "发送项目结构和关键文件给 agent 进行分析."
    (interactive)
    (when (project-current)
      (let* ((project-root (project-root (project-current)))
             (config-files (directory-files project-root t "\\.json\\|\\.yaml\\|\\.yml\\|\\.toml\\|\\.ini\\|Makefile" t))
             (readme-files (directory-files project-root t "README\\|readme\\|CHANGELOG\\|changelog" t)))
        (agent-shell-send-string (format "Project Analysis Request for: %s" project-root))
        (agent-shell-send-string "Please analyze this project structure and provide insights.")
        (when config-files
          (agent-shell-send-string "Configuration files found:")
          (dolist (file config-files)
            (when (and (file-regular-p file)
                       (< (nth 7 (file-attributes file)) 10000)) ; files < 10KB
              (agent-shell-send-file file))))
        (when readme-files
          (agent-shell-send-string "Documentation files:")
          (dolist (file readme-files)
            (when (file-regular-p file)
              (agent-shell-send-file file)))))))
  
  (defun riven/agent-shell-search-codebase (pattern)
    "在代码库中搜索模式并发送结果给 agent."
    (interactive "sSearch pattern: ")
    (when (project-current)
      (let* ((project-root (project-root (project-current)))
             (matches (split-string 
                      (shell-command-to-string 
                       (format "cd %s && grep -r -n --include='*.py' --include='*.js' --include='*.ts' --include='*.el' --include='*.go' '%s' . 2>/dev/null | head -20" 
                               (shell-quote-argument project-root) 
                               (shell-quote-argument pattern))) 
                      "\n" t)))
        (agent-shell-send-string (format "Codebase search results for '%s':" pattern))
        (dolist (match matches)
          (agent-shell-send-string match))
        (agent-shell-send-string "Please analyze these search results and provide insights."))))
  
  (defun riven/agent-shell-git-status ()
    "发送 git 状态给 agent 进行分析."
    (interactive)
    (when (and (fboundp 'magit-get-current-branch)
               (magit-get-current-branch))
      (let ((status (shell-command-to-string "git status --porcelain"))
            (branch (magit-get-current-branch))
            (diff (shell-command-to-string "git diff --stat HEAD")))
        (agent-shell-send-string (format "Git Status (Branch: %s):" branch))
        (when (string-match-p "^[^#]" status)
          (agent-shell-send-string "Modified files:")
          (agent-shell-send-string status))
        (when (string-match-p "." diff)
          (agent-shell-send-string "Diff summary:")
          (agent-shell-send-string diff))
        (agent-shell-send-string "Please analyze the git status and suggest next steps."))))
  
  ;; ============================================================
  ;; Agent Shell 文档和测试生成命令
  ;; ============================================================
  
  (defun riven/agent-shell-generate-docs ()
    "请求 agent 为当前项目生成文档."
    (interactive)
    (when (project-current)
      (riven/agent-shell-send-project-context)
      (agent-shell-send-string "Please generate comprehensive documentation for this project including:")
      (agent-shell-send-string "1. Project overview and purpose")
      (agent-shell-send-string "2. Architecture and key components")
      (agent-shell-send-string "3. Setup and installation instructions")
      (agent-shell-send-string "4. Usage examples and API documentation")
      (agent-shell-send-string "5. Development guidelines and contribution info")))
  
  (defun riven/agent-shell-generate-tests ()
    "为当前文件生成测试."
    (interactive)
    (if (buffer-file-name)
        (progn
          (agent-shell-send-string (format "File: %s" buffer-file-name))
          (agent-shell-send-string (format "Mode: %s" major-mode))
          (agent-shell-send-current-file)
          (agent-shell-send-string "Please generate comprehensive tests for this code.")
          (agent-shell-send-string "Include unit tests, integration tests, and edge cases."))
      (message "No file associated with current buffer")))
  
  (defun riven/agent-shell-lint-code ()
    "请求 agent 审查代码风格和 lint 问题."
    (interactive)
    (if (buffer-file-name)
        (progn
          (agent-shell-send-current-file)
          (agent-shell-send-string "Please review this code for:")
          (agent-shell-send-string "1. Code style and formatting issues")
          (agent-shell-send-string "2. Potential bugs and errors")
          (agent-shell-send-string "3. Performance optimizations")
          (agent-shell-send-string "4. Security vulnerabilities")
          (agent-shell-send-string "5. Best practices violations"))
      (message "No file associated with current buffer")))
  
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
    "Agent Shell 命令的 Transient 菜单."
    [:description
     (lambda () "Agent Shell Commands")

     ["Quick Start"
      ("c" "Start Claude Code" 
       (lambda () 
         (interactive)
         (call-interactively 'riven/start-claude-code)))
      ("u" "Start Cursor ACP"
       (lambda ()
         (interactive)
         (call-interactively 'riven/start-cursor-acp)))
      ("o" "Start OpenAI Codex"
       (lambda ()
         (interactive)
         (call-interactively 'riven/start-openai-codex)))]

     ["Basic Operations"
      ("n" "New shell" agent-shell-new-shell)
      ("N" "Start/reuse shell" agent-shell)
      ("t" "Toggle display" agent-shell-toggle)
      ("d" "Diagnose agents" riven/agent-shell-diagnose)]
     
     ["Install Tools"
      ("ic" "Install Claude Code" riven/install-claude-code)
      ("iu" "Install Cursor ACP" riven/install-cursor-agent-acp)]]

    [:if agent-shell-buffer-exists-p
     ["Code Operations"
      ("rf" "Review function" riven/agent-shell-code-review)
      ("ef" "Explain function" riven/agent-shell-explain-code)
      ("Ff" "Refactor function" riven/agent-shell-refactor-code)
      ("tf" "Add tests" riven/agent-shell-add-tests)
      ("sf" "Send function" riven/agent-shell-send-current-function)]
      
     ["Context"
      ("cb" "Send buffer context" riven/agent-shell-send-buffer-context)
      ("cp" "Send project context" riven/agent-shell-send-project-context)
      ("cr" "Send region" agent-shell-send-region)
      ("cf" "Send file" agent-shell-send-current-file)]
      
     ["Control"
      ("cc" "Interrupt" agent-shell-interrupt)
      ("cl" "Clear buffer" agent-shell-clear-buffer)
      ("ch" "Search history" agent-shell-search-history)]]

    [["Help & Quit"
      ("?" "Help" agent-shell-help-menu :if (lambda () (fboundp 'agent-shell-help-menu)))
      ("q" "Quit" transient-quit-one)]])
  
  (message "Agent-shell configuration loaded"))

;; ============================================================
;; 结束
;; ============================================================

(provide 'init-agent-shell)

;;; init-agent-shell.el ends here

