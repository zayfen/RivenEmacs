;;; keybindings-spec-ai.el --- AI/agent keybinding specs -*- lexical-binding: t; -*-

(defvar riven/keybindings-ai-spec
  '(("a" ai-code-menu "AI Code")
    ("c" gptel "Console")
    ("m" gptel-menu "Menu")
    ("s" gptel-send "Send")
    ("r" gptel-rewrite "Rewrite")
    ("t" riven/gptel-translate-region-review "Translate")
    ("S" riven/gptel-summarize-region-review "Summarize")
    ("w" gptel-rewrite-article "Write Article")
    ("d" gptel-extensions-ask-document "Ask Document")
    ("g" gptel-generate-commit-message "Generate Commit Message")
    ("M" riven/gptel-mcp-connect-popular "MCP Connect")
    ("V" riven/gptel-mcp-verify "MCP Verify"))
  "Declarative specs for commands under `C-c a'.")

(defvar riven/keybindings-agent-spec
  '(("=" ai-code-menu "AI Code Menu")
    ("1" ai-code-claude-code "Claude Code")
    ("2" ai-code-opencode "Open Code")
    ("3" ai-code-cursor-cli "Cursor CLI"))
  "Declarative specs for AI coding agent commands under `C-c ='.")

(provide 'keybindings-spec-ai)
