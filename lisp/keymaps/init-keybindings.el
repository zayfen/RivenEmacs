;; -*- coding: utf-8; lexical-binding: t -*-

(eval-and-compile
  (let ((current-file (or load-file-name byte-compile-current-file buffer-file-name)))
    (when current-file
      (add-to-list 'load-path (file-name-directory current-file)))))

(require 'cl-lib)
(require 'keybindings-engine)

(defun riven/keybindings-config ()
  "Apply all RivenEmacs keybindings from declarative specs."
  (require 'agent-shell nil t)
  (riven/keybindings-apply-leader-spec)
  (riven/keybindings-apply-open-query-ai)
  (riven/keybindings-apply-navigate)
  (riven/keybindings-apply-default-cleanups)
  (keymap-global-set "M-*" #'riven/agent-shell-dispatch))

(add-hook 'after-init-hook #'riven/keybindings-config)

(defun riven/lsp-bridge-keybindings ()
  "Set up lsp-bridge mode keybindings."
  (when (featurep 'lsp-bridge)
    (riven/keybindings-invoke-definer
     'leader-def
     (append (list :keymaps 'lsp-bridge-mode-map
                   :infix "c"
                   "" '(:ignore t :wk "Code"))
             (cl-mapcan
              (lambda (entry)
                (pcase-let ((`(,key ,cmd ,wk) entry))
                  (list key `(,cmd :wk ,wk))))
              riven/keybindings-lsp-spec)))))

(with-eval-after-load 'lsp-bridge
  (riven/lsp-bridge-keybindings))

(provide 'init-keybindings)
