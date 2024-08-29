;; -*- coding: utf-8; lexical-binding: t -*-

;; config project keybindings

(defun +consult-fd-in-home ()
         (interactive)
         "Find any file from home directory"
         (consult-fd "~/"))

(defun keybindings-config()
  (progn
       (leader-def
         :infix "p"
         "c" '(+remove-invalidate-buffers :wk "Clean invalidate buffers")
         "C" '(project-forget-zombie-projects :wk "Forget zombie projects")
         "f" '(project-find-file :wk "Find file in project")
         "g" '(+goto-file-at-point :wk "Goto file at point")
         "p" '(project-switch-project :wk "Switch project")
         "s" '(consult-ripgrep-ex :wk "Search symbol")
         "b" '(consult-project-buffer :wk "Buffers in project")
         "/" '(consult-git-grep :wk "Git grep")
         "r" '(color-rg-search-symbol-in-project :wk "Search/Replace"))

       (leader-def
         :infix "f"
         "d" '(crux-recentf-find-directory :wk "Find recent directory")
         "f" '(consult-fd :wk "Find file in directory")
         "l" '(+consult-fd-in-home :wk "Locate file")
         "r" '(consult-recent-file :wk "Recent files")
         "s" '(blink-search :wk "Search files"))

       (leader-def
         :infix "o"
         :wk "Open Toolbox"
         "d" '(docker :wk "Docker")
         "g" '(gptel :wk "GPT"))

       (global-unset-key (kbd "C-x l"))
       (lookup-leader-def
         "d" '(devdocs-lookup :wk "Lookup DevDocs")
         "g" '(gptel-send :wk "GPT send")
         "b" '(gptel-extensions-send-whole-buffer :wk "GPT send buffer")
         "w" '(gptel-extensions-ask-document :wk "GPT ask document"))
       ))


(add-hook 'after-init-hook #'keybindings-config)

(provide 'init-keybindings)
