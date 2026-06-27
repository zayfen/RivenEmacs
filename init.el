;; -*- coding: utf-8; lexical-binding: t -*-

(setq load-prefer-newer t)

(let* ((riven/root-dir (file-name-directory (file-truename (or load-file-name (buffer-file-name)))))
       (riven/lisp-dir (expand-file-name "lisp" riven/root-dir)))
  (add-to-list 'load-path riven/lisp-dir)
  (add-to-list 'load-path riven/root-dir)
  (dolist (dir (directory-files riven/lisp-dir t "^[^.].*"))
    (when (and (file-directory-p dir)
               (not (member (file-name-nondirectory dir) '("local"))))
      (add-to-list 'load-path dir)
      (dolist (sub (directory-files dir t "^[^.].*"))
        (when (file-directory-p sub)
          (add-to-list 'load-path sub)))))
  (let ((riven/writing-dir (expand-file-name "writing" riven/lisp-dir)))
    (when (file-directory-p riven/writing-dir)
      (add-to-list 'load-path riven/writing-dir))))

(use-package exec-path-from-shell
  :ensure t
  :defer 2
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
                 "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"
                 "HTTP_PROXY" "HTTPS_PROXY"
                 "GROQ_API_KEY" "DEEPSEEK_API_KEY" "OPENAI_API_KEY"
                 "ANTHROPIC_AUTH_TOKEN" "ANTHROPIC_BASE_URL"
                 "BRAVE_API_KEY" "TAVILY_API_KEY"
                 "GITHUB_TOKEN" "GITHUB_PERSONAL_ACCESS_TOKEN"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(load (expand-file-name "lisp/env.el" (file-name-directory (file-truename (or load-file-name (buffer-file-name))))) nil t)

(setq user-emacs-directory local-dir
      package-user-dir repo-dir
      package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(unless (fboundp 'riven/initialize-package-system)
  (defun riven/initialize-package-system ()
    "Initialize package.el after RivenEmacs package directories are configured."
    (require 'package)
    (unless package--initialized
      (package-initialize))))

(riven/initialize-package-system)

(defun setup-proxy ()
  "Setup network proxy using configuration management."
  (interactive)
  (rivenEmacs-setup-proxy))

(defun riven/load-core-modules ()
  "Load core modules required during startup."
  (require 'init-use-package)
  (require 'init-config)
  (require 'init-default)
  (require 'init-helper))

(defun riven/load-ui-modules ()
  "Load UI and editing foundation modules."
  (require 'init-theme)
  (require 'init-font)
  (require 'init-undo)
  (require 'init-autosave)
  (require 'init-which-key)
  (require 'init-hydra)
  (require 'init-minibuffer)
  (require 'init-completion-ui)
  (require 'init-consult)
  (require 'init-vertico)
  (require 'init-crux)
  (require 'init-editor)
  (require 'init-dired)
  (require 'init-format)
  (require 'init-jump)
  (require 'init-editorconfig)
  (require 'init-checker)
  (require 'init-pair)
  (require 'init-fold)
  (require 'init-markdown)
  (require 'init-treesit))

(defun riven/load-writing-modules ()
  "Load writing-related modules.
Org module itself is loaded lazily in org buffers."
  (require 'ews)
  (add-hook 'org-mode-hook (lambda () (require 'init-org))))

(defun riven/load-session-modules ()
  "Load session management modules."
  (require 'init-session))

(defun riven/load-tooling-modules ()
  "Load commonly used tooling modules."
  (require 'init-terminal))

(defun riven/load-keymap-modules ()
  "Load keymap aggregation module."
  (require 'init-keybindings))

(defun riven/load-deferred-modules ()
  "Load deferred modules on `emacs-startup-hook`."
  (require 'init-eglot)
  (require 'init-vc)
  (require 'init-debugger)
  (require 'init-envrc)
  (require 'init-project)
  (require 'init-gpt)
  (require 'init-ai-code)
  (require 'init-web)
  (require 'init-rust)
  (require 'init-python)
  (require 'init-java)
  (require 'init-swift)
  (require 'init-docker)
  (require 'init-quickrun)
  (require 'init-feed)
  (require 'init-lookup)
  (require 'init-reader)
  (require 'init-codeforces))

;; Keep startup behavior: immediate modules now, heavy modules deferred.
(riven/load-core-modules)
(riven/load-ui-modules)
(riven/load-writing-modules)
(riven/load-session-modules)
(riven/load-tooling-modules)
(add-hook 'emacs-startup-hook #'riven/load-deferred-modules 90)
(riven/load-keymap-modules)

(put 'narrow-to-region 'disabled nil)
