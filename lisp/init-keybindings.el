;; -*- coding: utf-8; lexical-binding: t -*-

;;; Code:
;; config project keybindings

(defun +consult-fd-in-home ()
  "Find file in home directory."
  (interactive)
  (consult-fd "~/"))

(defun +open-in-system-explorer ()
  "Open the current directory in the system's file explorer."
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (w32-shell-execute "explorer" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((eq system-type 'darwin) (shell-command "open ."))
   ((eq system-type 'gnu/linux) (shell-command "xdg-open ."))))

(defun +open-term-in-current-directory ()
  "Open a terminal in the current directory."
  (interactive)
  (let ((default-directory (if (buffer-file-name)
                               (file-name-directory (buffer-file-name))
                             default-directory)))
    (ansi-term "/usr/bin/zsh")))

(defun +show-current-buffer-path ()
  "Show the absolute path of the current buffer in the minibuffer."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (if file-path
        (message "%s" (file-truename file-path))
      (message "Current buffer is not visiting a file."))))


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
      "K" '(sp-rewrap-sexp :wk "Delete pair And Rewrap")
      "s" '(sp-delete-sexp :wk "Delete Sexp")
      "r" '(vr/replace :wk "Replace"))

    (open-leader-def
      "" '(:ignore t :wk "Open")
      "d" '(docker :wk "Docker")
      "e" '(+open-in-system-explorer :wk "Explorer")
      "n" '(elfeed :wk "News")
      "q" '(quickrun :wk "Quickrun")
      "t" '(+open-term-in-current-directory :wk "Terminal"))

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

    ;; Minibufer functions
    (leader-def
      :infix "m"
      "" '(:ignore t :wk "Minibuffer")
      "p" '(+show-current-buffer-path :wk "Show buffer path"))

    ))


(add-hook 'after-init-hook #'keybindings-config)

(provide 'init-keybindings)
