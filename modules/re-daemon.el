;;; re-daemon.el --- Daemon -*- lexical-binding: t; -*-

;; When we start in a non-daemon Emacs, we start a server whe Emacs is idle.
(+lazy-unless! (daemonp)
  (require 'server) ; For using `server-running-p'
  (unless (server-running-p)
    (let ((inhibit-message t))
      (+info! "Starting Emacs daemon in background.")
      (server-start nil t))))

;; Reload theme when creating a frame on the daemon
(add-hook
 'server-after-make-frame-hook
 (defun +daemon--reload-theme-h ()
   (load-theme rivenemacs-theme t)))

(add-hook
 'server-after-make-frame-hook
 (defun +daemon--reload-battery-mode-once-h ()
   (when (and (display-graphic-p)
              (bound-and-true-p display-battery-mode))
     (display-battery-mode -1)
     (display-battery-mode 1)
     (remove-hook 'server-after-make-frame-hook
                  #'+daemon--reload-battery-mode-once-h))))


(provide 're-daemon)
