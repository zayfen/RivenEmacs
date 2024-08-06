;; -*- coding: utf-8; lexical-binding: t -*-


;; load env from exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize))


(require 'init-default)
(require 'init-use-package)
(require 'init-theme)
(require 'init-font)
(require 'init-undo)
(require 'init-autosave)

;; init thirdparty packages
(require 'init-which-key)
(require 'init-general)


(require 'init-consult)
(require 'init-vertico)
(require 'init-crux)
(require 'init-embark)
(require 'init-editor)
(require 'init-format)
(require 'init-jump)
(require 'init-editorconfig)
(require 'init-checker)
(require 'init-pair)

;; important: tree-sitter
(require 'init-treesit)
(require 'init-lsp-bridge)

;; init git
(require 'init-vc)

;; Languages ;TODO
(require 'init-web)



;; keybindings
(require 'init-keybindings)
