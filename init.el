;; -*- coding: utf-8; lexical-binding: t -*-

;; load env from exec-path-from-shell
(use-package exec-path-from-shell
  :demand t
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "GROQ_API_KEY"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(require 'init-use-package)
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
(require 'init-embark)
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
(require 'init-lsp-bridge)

;; init git
(require 'init-vc)
(require 'init-debugger)

;; init env
(require 'init-envrc)

;; init project
(require 'init-project)

;; init devdocs
(require 'init-devdocs)
(require 'init-gpt)

;; Languages ;TODO
(require 'init-web)
(require 'init-rust)
(require 'init-python)

;; Tools
(require 'init-docker)
(require 'init-quickrun)
(require 'init-feed)
(require 'init-powerthesaurus)

;; keybindings
(require 'init-keybindings)
