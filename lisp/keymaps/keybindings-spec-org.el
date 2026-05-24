;;; keybindings-spec-org.el --- Org/session keybinding specs -*- lexical-binding: t; -*-

(defvar riven/keybindings-leader-spec-org
  '(("n" "Note"
     (("a" org-agenda-list "Agenda")
      ("c" org-capture "Capture")
      ("s" org-ql-search "Search notes")
      ("t" org-todo "Todo")
      ("d" org-deadline "Set Deadline")
      ("e" org-set-effort "Set Effort")
      ("p" org-set-property "Set Property")
      ("r" org-refile "Refile")
      ("l" org-insert-link "Insert Link")
      ("f" org-footnote-action "Footnote")
      ("x" org-export-dispatch "Export")
      ("w" report-last-week-tasks "Last Week Tasks"))))
  "Org declarative specs for structured `C-c' groups.")

(defvar riven/keybindings-leader-spec-session
  '(("x" "Session"
     (("s" rivenEmacs-session-save "Save session")
      ("l" rivenEmacs-session-load "Load session")
      ("r" rivenEmacs-session-reload "Reload session")
      ("c" rivenEmacs-session-current "Current session")
      ("k" rivenEmacs-session-clear "Clear session")
      ("d" rivenEmacs-session-delete "Delete session"))))
  "Session declarative specs for structured `C-c' groups.")

(provide 'keybindings-spec-org)
