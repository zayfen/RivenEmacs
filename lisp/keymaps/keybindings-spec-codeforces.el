;;; keybindings-spec-codeforces.el --- Codeforces keybinding specs -*- lexical-binding: t; -*-

(defvar riven/keybindings-codeforces-spec
  '(("l" codeforces-login "Login")
    ("L" codeforces-logout "Logout")
    ("p" codeforces-browse-problems "Browse problems"))
  "Declarative specs for Codeforces commands under `C-c f'.")

(provide 'keybindings-spec-codeforces)
