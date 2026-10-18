;; -*- coding: utf-8; lexical-binding: t -*-

(require 'cl-lib)
(require 'keybindings-engine)

(autoload 'ai-code-menu "ai-code" nil t)
(autoload 'dsh-emacs "dsh-emacs" nil t)

(defun riven/keybindings-config ()
  "Apply all RivenEmacs keybindings from declarative specs."
  (riven/keybindings-reset-owned-prefixes)
  (riven/keybindings-apply-leader-spec)
  (riven/keybindings-apply-agent-spec)
  (riven/keybindings-apply-navigate)
  (riven/keybindings-apply-default-cleanups)
  (riven/keybindings-configure-which-key-display)
  (keymap-global-set "M-*" #'ai-code-menu)
  ;; Direct top-level launcher (not a prefix group): the DeepSeek Harness
  ;; client loads from its autoload on the first press.
  (keymap-global-set "C-c d" #'dsh-emacs))

(add-hook 'after-init-hook #'riven/keybindings-config)

(provide 'init-keybindings)
