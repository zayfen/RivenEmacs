;; -*- coding: utf-8; lexical-binding: t -*-

(require 'cl-lib)
(require 'keybindings-engine)

(autoload 'ai-code-menu "ai-code" nil t)

(defun riven/keybindings-config ()
  "Apply all RivenEmacs keybindings from declarative specs."
  (riven/keybindings-reset-owned-prefixes)
  (riven/keybindings-apply-leader-spec)
  (riven/keybindings-apply-agent-spec)
  (riven/keybindings-apply-navigate)
  (riven/keybindings-apply-default-cleanups)
  (riven/keybindings-configure-which-key-display)
  (keymap-global-set "M-*" #'ai-code-menu))

(add-hook 'after-init-hook #'riven/keybindings-config)

(defun riven/eglot-register-code-prefix-which-key ()
  "Register human-readable which-key labels for `C-c c` code group."
  (when (fboundp 'which-key-add-keymap-based-replacements)
    (let ((replacements
           (append
            '("C-c c" "Code")
            (cl-mapcan
             (lambda (entry)
               (pcase-let ((`(,key ,_cmd ,wk) entry))
                 (list (format "C-c c %s" key) wk)))
             riven/keybindings-lsp-spec))))
      (apply #'which-key-add-keymap-based-replacements
             eglot-mode-map
             replacements))))

(defun riven/eglot-install-code-prefix ()
  "Install `C-c c` code prefix in `eglot-mode-map`."
  (let ((prefix-map (make-sparse-keymap)))
    (dolist (entry riven/keybindings-lsp-spec)
      (pcase-let ((`(,key ,cmd ,wk) entry))
        (ignore wk)
        (define-key prefix-map (kbd key) cmd)))
    (define-key eglot-mode-map (kbd "C-c c") prefix-map)))

(defun riven/eglot-keybindings ()
  "Set up Eglot mode keybindings."
  (when (featurep 'eglot)
    (riven/eglot-install-code-prefix)
    (riven/eglot-register-code-prefix-which-key)))

(with-eval-after-load 'eglot
  (riven/eglot-keybindings))

(provide 'init-keybindings)
