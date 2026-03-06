;;; keybindings-spec-core.el --- Core keybinding specs -*- lexical-binding: t; -*-

(defvar riven/keybindings-leader-spec-core
  '(("b" "Buffer"
     (("." +show-current-buffer-path "Show buffer path")
      ("-" narrow-to-region "Narrow region")
      ("=" widen "Widen narrow region")
      ("l" ibuffer-list-buffers "List buffers")
      ("p" previous-buffer "Prev buffer")
      ("m" bookmark-set "Set bookmark")
      ("n" next-buffer "Next buffer")
      ("q" read-only-mode "ReadOnly buffer")
      ("x" revert-buffer-quick "Revert buffer")
      ("h" vundo "History")))

    ("p" "Project"
     (("c" +remove-invalidate-buffers "Clean invalidate buffers")
      ("C" project-forget-zombie-projects "Forget zombie projects")
      ("f" project-find-file "Find file in project")
      ("g" +goto-file-at-point "Goto file at point")
      ("p" project-switch-project-ex "Switch project")
      ("s" consult-ripgrep-ex "Search symbol")
      ("b" consult-project-buffer "Buffers in project")
      ("/" consult-git-grep "Git grep")
      ("r" color-rg-search-symbol-in-project "Search/Replace")))

    ("f" "Find"
     (("d" crux-recentf-find-directory "Global Recent directory")
      ("f" consult-fd "Directory file")
      ("l" +consult-fd-in-home "Home file")
      ("r" consult-recent-file "Global Recent files")
      ("s" deadgrep "String")))

    ("g" "Git"
     (("b" magit-blame "Blame")
      ("d" magit-diff-dwim "Diff")
      ("c" magit-clone "Clone")
      ("g" magit-status "Status")
      ("h" git-hunk-toggle-mode "Hunk navigation")
      ("i" magit-init "Init")
      ("l" magit-log "Log")
      ("s" magit-stage "Stage")
      ("t" git-timemachine-toggle "Time machine")
      ("TAB" diff-hl-show-hunk "Show hunk")))

    ("k" "Kill"
     (("a" sp-splice-sexp "Delete Around")
      ("A" sp-rewrap-sexp "Delete Around and Rewrap")
      ("k" sp-kill-whole-line "Kill line(s) (smart)")
      ("r" vr/replace "Replace")
      ("s" sp-delete-sexp "Delete Sexp")
      ("z" zap-to-char "Zap to char")))

    ("\!" "Checker(Flymake)"
     (("l" flymake-show-buffer-diagnostics "List diagnostics")
      ("n" flymake-goto-next-error "Next error")
      ("p" flymake-goto-prev-error "Previous error")
      ("s" flymake-start "Start flymake")
      ("v" flymake-switch-to-log-buffer "View log")
      ("f" eslint-fix "Fix with ESLint"))))
  "Core declarative specs for `leader-def` groups.")

(defvar riven/keybindings-open-spec
  '(("d" docker "Docker")
    ("e" +open-in-system-explorer "Explorer")
    ("n" elfeed "News")
    ("q" quickrun "Quickrun")
    ("r" reader-open-doc "Reader")
    ("t" eat "Terminal"))
  "Declarative specs for `open-leader-def`.")

(defvar riven/keybindings-query-spec
  '(("d" devdocs-lookup "DevDocs")
    ("." fanyi-dwim "Dictionary")
    ("m" woman "Unix Manual")
    ("g" riven/google-search "Google")
    ("t" riven/google-translate "Google Translator"))
  "Declarative specs for `query-leader-def`.")

(defvar riven/keybindings-navigate-spec
  '(("c" avy-goto-char-2 "Goto Char")
    ("g" consult-goto-line "Line")
    ("i" consult-imenu "Imenu")
    ("l" link-hint-open-link "Link")
    ("o" consult-outline "Outline")
    ("m" consult-mark "Mark")
    ("M" consult-global-mark "Global Mark")
    ("n" next-error "Next Error")
    ("p" previous-error "Prev Error")
    ("[" diff-hl-previous-hunk "Previous Hunk")
    ("]" diff-hl-next-hunk "Next Hunk"))
  "Declarative specs for `navigate-leader-def`.")

(provide 'keybindings-spec-core)
