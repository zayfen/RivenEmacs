;; -*- coding: utf-8; lexical-binding: t -*-

(require 'cl-lib)
(require 'keybindings-engine)

(autoload 'ai-code-menu "ai-code" nil t)
(autoload 'dsh-emacs "dsh-emacs" nil t)

(defun riven/keybindings-config ()
  "Apply all RivenEmacs keybindings from declarative specs.
All AI entry points (gptel, `ai-code' agents, `dsh-emacs', MCP) live
under the single `C-c a' hub; the former `C-c =' and `C-c d' bindings
are retired."
  (riven/keybindings-reset-owned-prefixes)
  (riven/keybindings-apply-leader-spec)
  (riven/keybindings-apply-navigate)
  (riven/keybindings-apply-default-cleanups)
  (riven/keybindings-configure-which-key-display))

(add-hook 'after-init-hook #'riven/keybindings-config)

(provide 'init-keybindings)
