;;; init-agent-shell-install.el --- Agent installers -*- lexical-binding: t; -*-

(eval-and-compile
  (let ((current-file (or load-file-name byte-compile-current-file buffer-file-name)))
    (when current-file
      (add-to-list 'load-path (file-name-directory current-file)))))

(require 'init-agent-shell-core)

(defun riven/install-cursor-agent-acp ()
  "Install cursor-agent-acp via npm."
  (interactive)
  (if (not (executable-find "npm"))
      (message "错误: npm 未安装。请先安装 Node.js 和 npm。")
    (when (yes-or-no-p "是否要安装 cursor-agent-acp (需要 npm)? ")
      (message "正在安装 cursor-agent-acp...")
      (let ((result (shell-command "npm install -g @blowmage/cursor-agent-acp")))
        (if (= result 0)
            (progn
              (message "✓ cursor-agent-acp 安装成功!")
              (setq exec-path (append exec-path (list (expand-file-name "~/.nvm/versions/node/*/bin"))))
              t)
          (message "✗ cursor-agent-acp 安装失败。请手动运行: npm install -g @blowmage/cursor-agent-acp")
          nil)))))

(defun riven/install-claude-code ()
  "Install Claude Code via Homebrew (macOS only)."
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
            (progn (message "✓ Claude Code 安装成功!") t)
          (message "✗ Claude Code 安装失败。请手动运行: brew install --cask claude-code")
          nil))))))

(defun riven/install-opencode ()
  "Install Open Code via Homebrew (macOS only)."
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
            (progn (message "✓ Open Code 安装成功!") t)
          (message "✗ Open Code 安装失败。请手动运行: brew install anomalyco/tap/opencode")
          nil))))))

(defun riven/prompt-install-agent (agent-name install-function)
  "Prompt for AGENT-NAME installation via INSTALL-FUNCTION."
  (when (yes-or-no-p (format "%s 未安装。是否现在安装? " agent-name))
    (funcall install-function)))

(provide 'init-agent-shell-install)
