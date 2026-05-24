;;; init-agent-shell-install.el --- Agent installers -*- lexical-binding: t; -*-

(require 'init-agent-shell-core)
(require 'package)
(require 'package-vc)

(defconst riven/agent-shell-vc-package-recipes
  '((acp :url "https://github.com/xenodium/acp.el" :branch "main")
    (shell-maker :url "https://github.com/xenodium/shell-maker" :branch "main")
    (agent-shell :url "https://github.com/xenodium/agent-shell" :branch "main"))
  "VC package recipes used by agent-shell.")

(defconst riven/agent-shell-npm-package-recipes
  '((cursor-acp "cursor-agent-acp" "@blowmage/cursor-agent-acp@latest")
    (codex "codex" "@openai/codex@latest")
    (codex-acp "codex-acp" "@zed-industries/codex-acp@latest")
    (claude-acp "claude-agent-acp" "@zed-industries/claude-agent-acp@latest"))
  "Npm packages used by agent-shell and related AI agents.
Each entry is (ID EXECUTABLE PACKAGE-SPEC).")

(defun riven/agent-shell-package-desc (package-name)
  "Return installed package descriptor for PACKAGE-NAME."
  (car (alist-get package-name package-alist)))

(defun riven/agent-shell-upgrade-or-install-vc-package (package-name recipe)
  "Upgrade PACKAGE-NAME with RECIPE, installing it first if needed."
  (let ((pkg-desc (riven/agent-shell-package-desc package-name)))
    (if (and pkg-desc (package-vc-p pkg-desc))
        (package-vc-upgrade pkg-desc)
      (package-vc-install (cons package-name recipe)))))

(defun riven/agent-shell-npm-package-recipe (id)
  "Return npm package recipe identified by ID."
  (alist-get id riven/agent-shell-npm-package-recipes))

(defun riven/agent-shell-upgrade-npm-package (id)
  "Install or upgrade the npm package identified by ID."
  (if (not (executable-find "npm"))
      (progn
        (message "错误: npm 未安装，跳过 agent-shell npm 依赖升级。")
        nil)
    (pcase-let ((`(,executable ,package-spec)
                 (riven/agent-shell-npm-package-recipe id)))
      (unless package-spec
        (error "Unknown agent-shell npm package: %S" id))
      (let ((command (format "npm install -g %s" package-spec)))
        (message "正在升级 %s..." executable)
        (if (= (shell-command command) 0)
            (progn
              (message "✓ %s 已升级到最新版本。" executable)
              t)
          (message "✗ %s 升级失败。请手动运行: %s" executable command)
          nil)))))

(defun riven/agent-shell-upgrade-npm-packages ()
  "Install or upgrade npm packages required by agent-shell."
  (let ((ok 0)
        (fail 0))
    (if (not (executable-find "npm"))
        (progn
          (message "错误: npm 未安装，跳过 agent-shell npm 依赖升级。")
          nil)
      (dolist (entry riven/agent-shell-npm-package-recipes)
        (if (riven/agent-shell-upgrade-npm-package (car entry))
            (setq ok (1+ ok))
          (setq fail (1+ fail))))
      (message "agent-shell npm 依赖升级完成: %d 成功, %d 失败。" ok fail)
      (= fail 0))))

(defun riven/agent-shell-upgrade-cursor-acp ()
  "Upgrade Cursor ACP agent CLI to the latest npm version."
  (riven/agent-shell-upgrade-npm-package 'cursor-acp))

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
  "Upgrade agent-shell, VC dependencies, and npm AI agent packages."
  (interactive)
  (let ((npm-ok (riven/agent-shell-upgrade-npm-packages))
        (vc-ok (riven/agent-shell-upgrade-vc-packages)))
    (if (and npm-ok vc-ok)
        (message "✓ agent-shell setup 完成。重启 Emacs 后使用最新版本。")
      (message "agent-shell setup 完成，但存在失败项；请查看 *Messages*。"))
    (and npm-ok vc-ok)))

(defun riven/agent-shell-install-npm-package (id prompt-name)
  "Interactively install npm package ID after asking about PROMPT-NAME."
  (if (not (executable-find "npm"))
      (progn
        (message "错误: npm 未安装。请先安装 Node.js 和 npm。")
        nil)
    (when (yes-or-no-p (format "是否要安装 %s (需要 npm)? " prompt-name))
      (riven/agent-shell-upgrade-npm-package id))))

(defun riven/install-cursor-agent-acp ()
  "Install cursor-agent-acp via npm."
  (interactive)
  (riven/agent-shell-install-npm-package 'cursor-acp "cursor-agent-acp"))

(defun riven/install-codex ()
  "Install OpenAI Codex CLI via npm."
  (interactive)
  (riven/agent-shell-install-npm-package 'codex "codex"))

(defun riven/install-codex-acp ()
  "Install Codex ACP adapter via npm."
  (interactive)
  (riven/agent-shell-install-npm-package 'codex-acp "codex-acp"))

(defun riven/install-claude-agent-acp ()
  "Install Claude Agent ACP adapter via npm."
  (interactive)
  (riven/agent-shell-install-npm-package 'claude-acp "claude-agent-acp"))

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
