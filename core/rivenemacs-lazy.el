;; rivenemacs-lazy.el -*- lexical-binding: t; -*-



;; Virtual module loaded when idle after `rivenemacs-loaded'.
;; Used to synchronize loading some other stuff after loading Emacs.

;; The hooks in `rivenemacs-lazy-hook' are loaded incrementally when Emacs goes
;; idle, but when `rivenemacs-not-lazy' is set to t, they will be all loaded at
;; once.

;; Run hooks
(when rivenemacs-lazy-hook
  ;; Reverse the order to follow the order in which modules are loaded. Make
  ;; sure `gcmh-mode' is the last to be called. The `gc-cons-threshold' has been
  ;; set in "early-init.el" to a ridiculously high value to reduce the number of
  ;; garbage collections at startup, it will be overwritten by `gcmh-mode', so
  ;; we defer loading it to the end to maximize the benefit.
  (setq rivenemacs-lazy-hook (append (delq 'gcmh-mode (reverse rivenemacs-lazy-hook)) '(gcmh-mode)))
  (if rivenemacs-not-lazy
      (progn ; If `rivenemacs-no-lazy' is bound and true, force loading lazy hooks immediately
        (+log! "Loading %d lazy packages immediately."
               (length rivenemacs-lazy-hook))
        (run-hooks 'rivenemacs-lazy-hook))
    (+log! "Loading %d lazy packages incrementally." (length rivenemacs-lazy-hook))
    ;; Run hooks one by one, as a FIFO.
    (apply #'+eval-when-idle (append '(1) rivenemacs-lazy-hook))))

(+log! "Providing `rivenemacs-lazy'.")

(provide 'rivenemacs-lazy)
