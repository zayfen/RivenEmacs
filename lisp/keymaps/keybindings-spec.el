;;; keybindings-spec.el --- Aggregated keybinding specs -*- lexical-binding: t; -*-

(require 'keybindings-spec-core)
(require 'keybindings-spec-org)
(require 'keybindings-spec-ai)
(require 'keybindings-spec-lsp)

(defvar riven/keybindings-leader-spec
  (append riven/keybindings-leader-spec-core
          riven/keybindings-leader-spec-org
          riven/keybindings-leader-spec-session)
  "Final merged leader specs.")

(provide 'keybindings-spec)
