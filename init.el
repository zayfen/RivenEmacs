;; -*- coding: utf-8; lexical-binding: t -*-

;; load env from exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :defer 2
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "GROQ_API_KEY" "DEEPSEEK_API_KEY" "ANTHROPIC_AUTH_TOKEN" "ANTHROPIC_BASE_URL"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(defun setup-proxy ()
  "Setup network proxy using configuration management."
  (interactive)
  (rivenEmacs-setup-proxy))

;; export https_proxy=http://127.0.0.1:7890 http_proxy=http://127.0.0.1:7890 all_proxy=socks5://127.0.0.1:7890

(require 'init-use-package)
(require 'init-config)
(require 'init-default)
(require 'init-helper)
(require 'init-theme)
(require 'init-font)
(require 'init-undo)
(require 'init-autosave)

;; init thirdparty packages
(require 'init-which-key)
(require 'init-general)
(require 'init-hydra)

(require 'init-consult)
(require 'init-vertico)
(require 'init-crux)
;; (require 'init-embark) ;; I haven't usually use this package
(require 'init-editor)
(require 'init-dired)
(require 'init-format)
(require 'init-jump)
(require 'init-editorconfig)
(require 'init-checker)
(require 'init-pair)
(require 'init-fold)
(require 'init-markdown)

;; important: tree-sitter
(require 'init-treesit)

;; IDE
(require 'init-lsp-bridge)
;;(require 'init-eglot)
;;(require 'init-citre)

;; init git
(require 'init-vc)
(require 'init-debugger)
(require 'init-git-hunk)

;; init env
(require 'init-envrc)

;; init project
(require 'init-project)

;; init gpt
(require 'init-gpt)
(require 'init-agent-shell)

;; Languages
(require 'init-web)
(require 'init-rust)
(require 'init-python)
(require 'init-java)
(require 'init-swift)

;; Writing
(require 'ews)
(add-hook 'org-mode-hook (lambda () (require 'init-org)))

;; Tools
(require 'init-docker)
(require 'init-quickrun)
(require 'init-feed)
(require 'init-lookup)
(require 'init-terminal)
(require 'init-reader)

;; keybindings
(require 'init-keybindings)

(put 'narrow-to-region 'disabled nil)
