;;; keybindings-spec-ai.el --- AI/agent keybinding specs -*- lexical-binding: t; -*-

(defvar riven/keybindings-ai-spec
  '(;; Chat and text actions (gptel).
    ("c" gptel "Chat")
    ("m" gptel-menu "Menu")
    ("s" gptel-send "Send")
    ("r" gptel-rewrite "Rewrite")
    ("R" gptel-extensions-refactor "Refactor")
    ("t" riven/gptel-translate-region-review "Translate")
    ("S" riven/gptel-summarize-region-review "Summarize")
    ("w" gptel-rewrite-article "Write Article")
    ("W" riven/gptel-work "Work Mode")
    ("D" gptel-extensions-ask-document "Ask Document")
    ;; CLI coding agents (ai-code).
    ("a" ai-code-menu "Agent Menu")
    ("1" ai-code-codex-cli "Codex")
    ("2" ai-code-opencode "OpenCode")
    ("3" ai-code-cursor-cli "Cursor CLI")
    ("4" ai-code-claude-code "Claude Code")
    ("5" ai-code-aider-cli "Aider")
    ;; DeepSeek Harness client (dsh-emacs).
    ("d" dsh-emacs "DeepSeek Harness")
    ;; MCP tool setup; verify also connects, so one command is enough.
    ("M" riven/gptel-mcp-verify "MCP (connect + verify)"))
  "Declarative specs for commands under `C-c a'.
This is the single AI hub: gptel chat/text actions, `ai-code' CLI
agents, the `dsh-emacs' DeepSeek Harness client, and MCP setup.
The former `C-c =' agent group and the `C-c d' DSH launcher were
merged here, and commit-message generation moved to the Git group
\(`C-c g G').")

(provide 'keybindings-spec-ai)
;;; keybindings-spec-ai.el ends here
