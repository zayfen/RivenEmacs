;;; keybindings-spec-ai.el --- AI/agent keybinding specs -*- lexical-binding: t; -*-

(defvar riven/keybindings-ai-spec
  '(("A" ai-code-menu "AI Code")
    ("." gptel "Console")
    ("m" gptel-menu "Menu")
    ("=" gptel-send "Send")
    ("M" riven/gptel-mcp-connect-popular "MCP Connect")
    ("V" riven/gptel-mcp-verify "MCP Verify")
    ("r" gptel-rewrite "Rewrite")
    ("t" riven/gptel-translate-region-review "Translate")
    ("?" gptel-extensions-ask-document "Ask Document")
    ("w" gptel-rewrite-article "Write Article")
    ("s" riven/gptel-summarize-region-review "Summarize Document")
    ("q" gptel-query-devdoc "Query DevDoc")
    ("g" gptel-generate-commit-message "Generate Commit Message"))
  "Declarative specs for `ai-leader-def`.")

(defvar riven/keybindings-agent-spec
  '(("=" agent-shell "Start/Reuse Agent Shell")
    ("1" riven/start-claude-code "Start Claude Code")
    ("2" riven/start-open-code "Start Open Code")
    ("3" riven/start-cursor-acp "Start Cursor ACP")
    ("s" agent-shell-setup "Setup/Upgrade Agent Shell"))
  "Declarative specs for commands under `C-c ='.")

(provide 'keybindings-spec-ai)
