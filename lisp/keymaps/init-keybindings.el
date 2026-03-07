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

(defun riven/lsp-bridge-register-code-prefix-which-key ()
  "Register human-readable which-key labels for `C-c c` code group."
  (when (fboundp 'which-key-add-keymap-based-replacements)
    (apply #'which-key-add-keymap-based-replacements
           lsp-bridge-mode-map
           (append
            '("C-c c" "Code")
            (cl-mapcan
             (lambda (entry)
               (pcase-let ((`(,key ,_cmd ,wk) entry))
                 (list (format "C-c c %s" key) wk)))
             riven/keybindings-lsp-spec)))))

(defun riven/lsp-bridge-install-code-prefix-fallback ()
  "Install a stable `C-c c` code prefix in `lsp-bridge-mode-map`."
  (let ((prefix-map (make-sparse-keymap)))
    (dolist (entry riven/keybindings-lsp-spec)
      (pcase-let ((`(,key ,cmd ,wk) entry))
        (define-key prefix-map (kbd key) `(menu-item ,wk ,cmd))))
    (define-key lsp-bridge-mode-map (kbd "C-c c") (cons "Code" prefix-map))))

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
              riven/keybindings-lsp-spec)))
    ;; Keep group prefix available even if dynamic leader injection is skipped.
    (riven/lsp-bridge-install-code-prefix-fallback)
    (riven/lsp-bridge-register-code-prefix-which-key)))

(with-eval-after-load 'lsp-bridge
  (riven/lsp-bridge-keybindings))

(provide 'init-keybindings)
