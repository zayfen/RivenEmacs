;; -*- coding: utf-8; lexical-binding: t -*-

;; config project keybindings

(defun +consult-fd-in-home ()
         (interactive)
         "Find any file from home directory"
         (consult-fd "~/"))

(defun +open-in-system-explorer ()
  "Open the current directory in the system's file explorer."
  (interactive)
  (cond
   ((eq system-type 'windows-nt) (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((eq system-type 'darwin) (shell-command "open ."))
   ((eq system-type 'gnu/linux) (shell-command "xdg-open ."))))

(defun +open-term-in-current-directory ()
  "Open a terminal in the current directory."
  (interactive)
  (let ((default-directory (if (buffer-file-name)
                               (file-name-directory (buffer-file-name))
                             default-directory)))
    (ansi-term "/usr/bin/zsh"))) ; Replace "/bin/bash" with your preferred shell (e.g., "/bin/zsh")

(defun keybindings-config()
  (progn
       (leader-def
         :infix "p"
         "" '(:ignore t :wk "Project")
         "c" '(+remove-invalidate-buffers :wk "Clean invalidate buffers")
         "C" '(project-forget-zombie-projects :wk "Forget zombie projects")
         "f" '(project-find-file :wk "Find file in project")
         "g" '(+goto-file-at-point :wk "Goto file at point")
         "p" '(project-switch-project-ex :wk "Switch project")
         "s" '(consult-ripgrep-ex :wk "Search symbol")
         "b" '(consult-project-buffer :wk "Buffers in project")
         "/" '(consult-git-grep :wk "Git grep")
         "r" '(color-rg-search-symbol-in-project :wk "Search/Replace"))

       (leader-def
         :infix "f"
         "" '(:ignore t :wk "Find")
         "d" '(crux-recentf-find-directory :wk "Find recent directory")
         "f" '(consult-fd :wk "Find file in directory")
         "l" '(+consult-fd-in-home :wk "Locate file")
         "r" '(consult-recent-file :wk "Recent files")
         "s" '(blink-search :wk "Search files"))

       (leader-def
         :infix "k"
         "" '(:ignore t :wk "Kill")
         "k" '(sp-splice-sexp :wk "Delete pair")
         "s" '(sp-delete-sexp :wk "Delete Sexp")
         "r" '(sp-rewrap-sexp :wk "Delete pair And Rewrap"))

       (open-leader-def
         "" '(:ignore t :wk "Open")
         "d" '(docker :wk "Docker")
         "e" '(+open-in-system-explorer :wk "Explorer")
         "n" '(elfeed :wk "News")
         "q" '(quickrun :wk "Quickrun")
         "t" '(+onpen-term-in-current-directory :wk "Terminal"))

       (lookup-leader-def
         "" '(:ignore t :wk "Lookup")
         "d" '(devdocs-lookup :wk "Lookup DevDocs")
         "p" '(powerthesaurus-lookup-dwim :wk "Lookup PowerThesaurus")
         "." '(fanyi-dwim :wk "Dictionary")
         "t" '(gt-do-translate :wk "Translate Sentence/Buffer"))

       (gpt-leader-def
        "" '(:ignore t :wk "GPT")
        "g" '(gptel :wk "GPT")
        "s" '(gptel-send :wk "GPT send")
        "b" '(gptel-extensions-send-whole-buffer :wk "GPT send buffer")
        "t" '(gptel-translate-to-langs :wk "Translate to many languages")
        "w" '(gptel-extensions-ask-document :wk "GPT ask document"))

       ;; define prefix lable
       (leader-def
         "\!" '(:ignore t :wk "Checker(Flycheck)")
         "&" '(:ignore t :wk "Yasnippet")
         "@" '(:ignore t :wk "Hideshow"))

       (leader-def
         :infix "r"
         "" '(vr/replace :wk "+Replace"))

       ))


(add-hook 'after-init-hook #'keybindings-config)

(provide 'init-keybindings)
