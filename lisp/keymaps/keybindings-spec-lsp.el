;;; keybindings-spec-lsp.el --- LSP keybinding specs -*- lexical-binding: t; -*-

(defun riven/eglot-code-actions ()
  "Run Eglot code actions at point."
  (interactive)
  (require 'eglot nil t)
  (if (fboundp 'eglot-code-actions)
      (call-interactively 'eglot-code-actions)
    (user-error "Eglot code actions are unavailable")))

(defun riven/eglot-format-buffer ()
  "Format current buffer with Eglot if available."
  (interactive)
  (require 'eglot nil t)
  (cond
   ((fboundp 'eglot-format-buffer)
    (call-interactively 'eglot-format-buffer))
   ((fboundp 'eglot-format)
    (call-interactively 'eglot-format))
   (t
    (user-error "Eglot format is unavailable"))))

(defun riven/eglot-find-implementation ()
  "Find implementation with Eglot, fallback to xref definitions."
  (interactive)
  (require 'eglot nil t)
  (if (fboundp 'eglot-find-implementation)
      (call-interactively 'eglot-find-implementation)
    (call-interactively 'xref-find-definitions)))

(defun riven/eglot-show-documentation ()
  "Show symbol documentation in a dedicated buffer."
  (interactive)
  (cond
   ((fboundp 'eldoc-doc-buffer)
    (call-interactively 'eldoc-doc-buffer))
   ((fboundp 'eldoc-print-current-symbol-info)
    (call-interactively 'eldoc-print-current-symbol-info))
   (t
    (user-error "No documentation command available"))))

(defvar riven/flymake-diagnostics-quick-close-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") #'riven/flymake-close-diagnostics-or-keyboard-quit)
    map)
  "Keymap for `riven/flymake-diagnostics-quick-close-mode'.")

(define-minor-mode riven/flymake-diagnostics-quick-close-mode
  "Bind `C-g` to close Flymake diagnostics window before normal quit."
  :init-value nil
  :lighter nil
  :keymap riven/flymake-diagnostics-quick-close-mode-map)

(defun riven/flymake-diagnostics-window (&optional source-buffer)
  "Return Flymake diagnostics window for SOURCE-BUFFER, or nil."
  (let ((buf (or source-buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((name (if (fboundp 'flymake--diagnostics-buffer-name)
                        (flymake--diagnostics-buffer-name)
                      "*Flymake diagnostics*")))
          (get-buffer-window name t))))))

(defun riven/flymake-close-diagnostics-or-keyboard-quit ()
  "Close Flymake diagnostics window for current buffer, else run `keyboard-quit'."
  (interactive)
  (let ((win (riven/flymake-diagnostics-window)))
    (if (window-live-p win)
        (progn
          (delete-window win)
          (riven/flymake-diagnostics-quick-close-mode -1))
      (riven/flymake-diagnostics-quick-close-mode -1)
      (keyboard-quit))))

(defun riven/flymake-show-buffer-diagnostics-focus ()
  "Show Flymake diagnostics and focus the diagnostics window."
  (interactive)
  (require 'flymake nil t)
  (if (fboundp 'flymake-show-buffer-diagnostics)
      (progn
        (let ((source (current-buffer)))
          (with-current-buffer source
            (riven/flymake-diagnostics-quick-close-mode 1)))
        (call-interactively 'flymake-show-buffer-diagnostics)
        (let ((win (riven/flymake-diagnostics-window)))
          (when (window-live-p win)
            (select-window win))))
    (user-error "Flymake diagnostics command is unavailable")))

(defun riven/eglot-rename ()
  "Rename symbol with Eglot."
  (interactive)
  (require 'eglot nil t)
  (if (fboundp 'eglot-rename)
      (call-interactively 'eglot-rename)
    (user-error "Eglot rename is unavailable")))

(defun riven/eglot-find-type-definition ()
  "Find type definition with Eglot, fallback to xref definitions."
  (interactive)
  (require 'eglot nil t)
  (if (fboundp 'eglot-find-typeDefinition)
      (call-interactively 'eglot-find-typeDefinition)
    (call-interactively 'xref-find-definitions)))

(defvar riven/keybindings-lsp-spec
  '(("a" riven/eglot-code-actions "Code Actions")
    ("e" riven/flymake-show-buffer-diagnostics-focus "Diagnostic List")
    ("d" xref-find-definitions "Find Definition")
    ("f" riven/eglot-format-buffer "Format Buffer")
    ("i" riven/eglot-find-implementation "Find Implementation")
    ("k" riven/eglot-show-documentation "Show Documentation")
    ("p" xref-find-definitions-other-window "Peek Definition")
    ("q" eslint-fix "Quick Fix (ESLint)")
    ("r" riven/eglot-rename "Rename Symbol")
    ("t" riven/eglot-find-type-definition "Find Type Definition")
    ("?" xref-find-references "Find References"))
  "Declarative specs for lsp keybindings.")

(provide 'keybindings-spec-lsp)
