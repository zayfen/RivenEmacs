;; -*- coding: utf-8; lexical-binding: t -*-

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
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "GROQ_API_KEY" "DEEPSEEK_API_KEY" "ANTHROPIC_AUTH_TOKEN" "ANTHROPIC_BASE_URL"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(load (expand-file-name "lisp/env.el" (file-name-directory (file-truename (or load-file-name (buffer-file-name))))) nil t)

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
  (require 'init-general)
  (require 'init-hydra)
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

(defun riven/load-ai-modules ()
  "Load AI and agent integration modules needed early."
  (require 'init-agent-shell))

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
  (require 'init-lsp-bridge)
  (require 'init-vc)
  (require 'init-debugger)
  (require 'init-envrc)
  (require 'init-project)
  (require 'init-gpt)
  (require 'init-web)
  (require 'init-rust)
  (require 'init-python)
  (require 'init-java)
  (require 'init-swift)
  (require 'init-docker)
  (require 'init-quickrun)
  (require 'init-feed)
  (require 'init-lookup)
  (require 'init-reader))

;; Keep startup behavior: immediate modules now, heavy modules deferred.
(riven/load-core-modules)
(riven/load-ui-modules)
(riven/load-ai-modules)
(riven/load-writing-modules)
(riven/load-session-modules)
(riven/load-tooling-modules)
(add-hook 'emacs-startup-hook #'riven/load-deferred-modules 90)
(riven/load-keymap-modules)

(put 'narrow-to-region 'disabled nil)
