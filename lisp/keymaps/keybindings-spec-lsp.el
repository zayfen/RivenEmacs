;;; keybindings-spec-lsp.el --- LSP keybinding specs -*- lexical-binding: t; -*-

(defun riven/eglot-format-dispatch ()
  "Format current buffer via the available Eglot command."
  (interactive)
  (cond
   ((fboundp 'eglot-format-buffer)
    (call-interactively #'eglot-format-buffer))
   ((fboundp 'eglot-format)
    (call-interactively #'eglot-format))
   (t
    (user-error "Eglot format command is unavailable"))))

(defun riven/eglot-find-type-definition-dispatch ()
  "Find type definition with Eglot, fallback to xref definitions."
  (interactive)
  (cond
   ((fboundp 'eglot-find-typeDefinition)
    (call-interactively #'eglot-find-typeDefinition))
   ((fboundp 'eglot-find-type-definition)
    (call-interactively #'eglot-find-type-definition))
   (t
    (call-interactively #'xref-find-definitions))))

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

(defun riven/eldoc-doc-buffer-focus ()
  "Show ElDoc documentation buffer and move point to its window."
  (interactive)
  (unless (and (boundp 'eldoc--doc-buffer)
               (buffer-live-p eldoc--doc-buffer))
    (when (fboundp 'eldoc-print-current-symbol-info)
      (call-interactively #'eldoc-print-current-symbol-info)))
  (call-interactively #'eldoc-doc-buffer)
  (when (and (boundp 'eldoc--doc-buffer)
             (buffer-live-p eldoc--doc-buffer))
    (let ((win (get-buffer-window eldoc--doc-buffer t)))
      (when (window-live-p win)
        (select-window win)))))

(defvar riven/keybindings-lsp-spec
  '(("a" eglot-code-actions "Code Actions")
    ("e" riven/flymake-show-buffer-diagnostics-focus "Diagnostic List")
    ("d" riven/xref-find-definitions-or-search "Find Definition")
    ("f" riven/eglot-format-dispatch "Format Buffer")
    ("i" eglot-find-implementation "Find Implementation")
    ("k" riven/eldoc-doc-buffer-focus "Show Documentation")
    ("p" xref-find-definitions-other-window "Peek Definition")
    ("q" eslint-fix "Quick Fix (ESLint)")
    ("r" eglot-rename "Rename Symbol")
    ("t" riven/eglot-find-type-definition-dispatch "Find Type Definition")
    ("?" xref-find-references "Find References"))
  "Declarative specs for lsp keybindings.")

(provide 'keybindings-spec-lsp)
