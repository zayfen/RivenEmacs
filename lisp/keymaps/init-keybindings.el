;; -*- coding: utf-8; lexical-binding: t -*-

(require 'cl-lib)
(require 'keybindings-engine)

(autoload 'ai-code-menu "ai-code" nil t)

(defun riven/keybindings-config ()
  "Apply all RivenEmacs keybindings from declarative specs."
  (riven/keybindings-reset-owned-prefixes)
  (riven/keybindings-apply-leader-spec)
  (riven/keybindings-apply-agent-spec)
  (riven/keybindings-apply-navigate)
  (riven/keybindings-apply-default-cleanups)
  (riven/keybindings-configure-which-key-display)
  (keymap-global-set "M-*" #'ai-code-menu))

(add-hook 'after-init-hook #'riven/keybindings-config)

(provide 'init-keybindings)
