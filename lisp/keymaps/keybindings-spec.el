;;; keybindings-spec.el --- Aggregated keybinding specs -*- lexical-binding: t; -*-

(require 'keybindings-commands)
(require 'keybindings-spec-core)
(require 'keybindings-spec-org)
(require 'keybindings-spec-ai)
(require 'keybindings-spec-lsp)
(require 'keybindings-spec-codeforces)

(defvar riven/keybindings-leader-spec
  (append riven/keybindings-leader-spec-core
          `(("a" "AI" ,riven/keybindings-ai-spec))
          `(("f" "Codeforces" ,riven/keybindings-codeforces-spec))
          riven/keybindings-leader-spec-org
          riven/keybindings-leader-spec-session)
  "Final merged leader specs.")

(provide 'keybindings-spec)
