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
      (condition-case nil
          (apply #'which-key-add-keymap-based-replacements
                 eglot-mode-map
                 replacements)
        (wrong-type-argument
         (apply #'which-key-add-keymap-based-replacements
                'eglot-mode-map
                replacements))))))

(defun riven/eglot-install-code-prefix-fallback ()
  "Install a stable `C-c c` code prefix in `eglot-mode-map`."
  (let ((prefix-map (make-sparse-keymap)))
    (dolist (entry riven/keybindings-lsp-spec)
      (pcase-let ((`(,key ,cmd ,wk) entry))
        (ignore wk)
        (define-key prefix-map (kbd key) cmd)))
    (define-key eglot-mode-map (kbd "C-c c") prefix-map)))

(defun riven/eglot-keybindings ()
  "Set up Eglot mode keybindings."
  (when (featurep 'eglot)
    ;; Avoid `general` dynamic keymap delay path here; it can treat
    ;; keymap values as symbols and raise `wrong-type-argument symbolp`.
    (riven/eglot-install-code-prefix-fallback)
    (riven/eglot-register-code-prefix-which-key)))

(with-eval-after-load 'eglot
  (riven/eglot-keybindings))

(provide 'init-keybindings)
