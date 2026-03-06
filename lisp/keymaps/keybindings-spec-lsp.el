;;; keybindings-spec-lsp.el --- LSP keybinding specs -*- lexical-binding: t; -*-

(defvar riven/keybindings-lsp-spec
  '(("a" lsp-bridge-code-action "Code actions")
    ("e" lsp-bridge-diagnostic-list "Diagnostic list")
    ("d" lsp-bridge-find-def-ex "Find define")
    ("f" lsp-bridge-code-format "Format code")
    ("i" lsp-bridge-find-impl "Find implementation")
    ("k" lsp-bridge-popup-documentation "Find Document")
    ("p" lsp-bridge-peek "Peek")
    ("q" eslint-fix "QuickFix (Eslint)")
    ("r" lsp-bridge-rename "Rename")
    ("t" lsp-bridge-find-type-def "Find type definition")
    ("?" lsp-bridge-find-references "Find References"))
  "Declarative specs for lsp keybindings.")

(provide 'keybindings-spec-lsp)
