;;; keybindings-spec-org.el --- Org/session keybinding specs -*- lexical-binding: t; -*-

(defvar riven/keybindings-leader-spec-org
  '(("n" "Note"
     (("a" org-agenda-list "Agenda List")
      ("d" org-deadline "Set Deadline")
      ("e" org-set-effort "Set Effort")
      ("f" org-footnote-action "Footnote")
      ("i" org-capture "Capture")
      ("l" org-insert-link "Insert Link")
      ("m" org-mark-subtree "Mark Subtree")
      ("p" org-set-property "Set Property")
      ("r" org-refile "Refile")
      ("s" org-ql-search "Search")
      ("t" org-todo "Toggle TODO")
      ("u" org-update-statistics-cookies "Update Stats")
      ("v" org-show-todo-tree "TODO Tree")
      ("w" report-last-week-tasks "Last Week Tasks")
      ("x" org-export-dispatch "Export")
      ("+" org-shifttab "Cycle Visibility")
      ("*" org-ctrl-c-star "Insert Heading"))))
  "Org declarative specs for `leader-def` groups.")

(defvar riven/keybindings-leader-spec-session
  '(("s" "Session"
     (("s" easysession-switch-to "Switch session")
      ("S" easysession-save-as "Save as new")
      ("l" easysession-load "Load session")
      ("L" easysession-load-including-geometry "Load with geometry")
      ("d" easysession-delete "Delete session")
      ("r" easysession-rename "Rename session")
      ("." easysession-save "Save current")
      ("c" easysession-get-current-session-name "Current session"))))
  "Session declarative specs for `leader-def` groups.")

(provide 'keybindings-spec-org)
