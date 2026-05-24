;;; init-agent-shell-install.el --- Agent installers -*- lexical-binding: t; -*-

(require 'init-agent-shell-core)
(require 'package)
(require 'package-vc)

(defconst riven/agent-shell-vc-package-recipes
  '((acp :url "https://github.com/xenodium/acp.el" :branch "main")
    (shell-maker :url "https://github.com/xenodium/shell-maker" :branch "main")
    (agent-shell :url "https://github.com/xenodium/agent-shell" :branch "main"))
  "VC package recipes used by agent-shell.")

(defun riven/agent-shell-package-desc (package-name)
  "Return installed package descriptor for PACKAGE-NAME."
  (car (alist-get package-name package-alist)))

(defun riven/agent-shell-upgrade-or-install-vc-package (package-name recipe)
  "Upgrade PACKAGE-NAME with RECIPE, installing it first if needed."
  (let ((pkg-desc (riven/agent-shell-package-desc package-name)))
    (if (and pkg-desc (package-vc-p pkg-desc))
        (package-vc-upgrade pkg-desc)
      (package-vc-install (cons package-name recipe)))))

(defun riven/agent-shell-upgrade-cursor-acp ()
  "Upgrade Cursor ACP agent CLI to the latest npm version."
  (if (not (executable-find "npm"))
      (progn
        (message "错误: npm 未安装，跳过 cursor-agent-acp 升级。")
        nil)
    (message "正在升级 cursor-agent-acp...")
    (let ((result (shell-command "npm install -g @blowmage/cursor-agent-acp@latest")))
      (if (= result 0)
          (progn
            (message "✓ cursor-agent-acp 已升级到最新版本。")
            t)
        (message "✗ cursor-agent-acp 升级失败。请手动运行: npm install -g @blowmage/cursor-agent-acp@latest")
        nil))))

(defun riven/agent-shell-upgrade-vc-packages ()
  "Upgrade agent-shell and its VC dependencies to their latest revisions."
  (let ((ok 0)
        (fail 0))
    (dolist (entry riven/agent-shell-vc-package-recipes)
      (let ((pkg (car entry))
            (recipe (cdr entry)))
        (condition-case err
            (progn
              (message "正在升级 %s..." pkg)
              (riven/agent-shell-upgrade-or-install-vc-package pkg recipe)
              (setq ok (1+ ok)))
          (error
           (setq fail (1+ fail))
           (message "✗ %s 升级失败: %s" pkg (error-message-string err))))))
    (message "agent-shell VC 包升级完成: %d 成功, %d 失败。" ok fail)
    (= fail 0)))

(defun agent-shell-setup ()
  "Upgrade Cursor ACP, agent-shell, and agent-shell VC dependencies."
  (interactive)
  (let ((cursor-ok (riven/agent-shell-upgrade-cursor-acp))
        (vc-ok (riven/agent-shell-upgrade-vc-packages)))
    (if (and cursor-ok vc-ok)
        (message "✓ agent-shell setup 完成。重启 Emacs 后使用最新版本。")
      (message "agent-shell setup 完成，但存在失败项；请查看 *Messages*。"))
    (and cursor-ok vc-ok)))

(defun riven/install-cursor-agent-acp ()
  "Install cursor-agent-acp via npm."
  (interactive)
  (if (not (executable-find "npm"))
      (message "错误: npm 未安装。请先安装 Node.js 和 npm。")
    (when (yes-or-no-p "是否要安装 cursor-agent-acp (需要 npm)? ")
      (message "正在安装 cursor-agent-acp...")
      (let ((result (shell-command "npm install -g @blowmage/cursor-agent-acp@latest")))
        (if (= result 0)
            (progn
              (message "✓ cursor-agent-acp 安装成功!")
              (setq exec-path (append exec-path (list (expand-file-name "~/.nvm/versions/node/*/bin"))))
              t)
          (message "✗ cursor-agent-acp 安装失败。请手动运行: npm install -g @blowmage/cursor-agent-acp@latest")
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
