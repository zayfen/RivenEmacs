;;; -*- coding: utf-8; lexical-binding: t -*-



(use-package dape
  :vc (:url "https://github.com/svaante/dape")
  ;; Load dape now: this module is loaded from `after-init-hook' (depth -20),
  ;; so registering an `after-init-hook' activation here would run too late.
  :demand t
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  (setq dape-key-prefix "\C-cd")

  :hook
  ;; Save breakpoints on quit
  ((kill-emacs . dape-breakpoint-save))

  :config
  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; Global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-start-hook 'dape-info)
  ;; (remove-hook 'dape-start-hook 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-stopped-hook 'dape-info)
  ;; (add-hook 'dape-stopped-hook 'dape-repl)

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Load saved breakpoints now (after-init-hook already ran).
  (when (fboundp 'dape-breakpoint-load)
    (dape-breakpoint-load))

  ;; Project root as the working directory.
  (setq dape-cwd-function #'get-project-root))







(provide 'init-debugger)
