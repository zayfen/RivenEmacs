;; re-splash.el --- RivenEmacs -*- lexical-binding: t; -*-




;; Adapted from: github.com/rougier/emacs-splash

(setq inhibit-startup-screen t)
(defvar rivenemacs-splash-buffer-name "*rivenemacs-splash*")

(defun rivenemacs-splash ()
  "RivenEmacs splash screen"
  ;; If there are buffer associated with filenames, we don't show splash screen.
  (when (zerop (length (seq-filter #'identity (mapcar #'buffer-file-name (buffer-list)))))
    (let* ((buffer (get-buffer-create rivenemacs-splash-buffer-name))
           (height (- (window-body-height nil) 1))
           (padding-center (min 5 (- (/ height 3) 1)))
           (padding-bottom (min 2 (- height (/ height 3) 3))))
      (with-current-buffer buffer
        (erase-buffer)
        ;; Buffer local settings
        (setq-local cursor-type nil
                    vertical-scroll-bar nil
                    horizontal-scroll-bar nil)

        ;; Vertical padding to center
        (insert-char ?\n padding-center)

        ;; Central text
        (insert-char ?\s 10)
        (insert (propertize "RivenEmacs" 'face 'bold))
        (insert-char ?\n)
        (insert-char ?\s 10)
        (insert (propertize
                 (format "Running GNU Emacs %s%s"
                         emacs-version
                         (if emacs-repository-version
                             (format " (%s)" (substring emacs-repository-version 0 10))
                           ""))
                 'face 'shadow))

        ;; Bootstraping
        (unless (file-exists-p (concat rivenemacs-local-dir "straight/repos/straight.el/bootstrap.el"))
          (insert-char ?\n)
          (insert-char ?\s 10)
          (insert (propertize "You are running RivenEmacs for the first time."
                              'face 'warning))
          (insert-char ?\n)
          (insert-char ?\s 10)
          (insert (propertize "Please wait while RivenEmacs installs the required packages."
                              'face 'warning)))

        ;; Vertical padding to bottom
        (insert-char ?\n padding-bottom)

        ;; Copyright text
        (insert-char ?\n)
        (insert-char ?\s 10)
        (insert (propertize "Minimal Emacs configuration for daily use" 'face 'shadow))
        (insert-char ?\n)
        (insert-char ?\s 10)
        (insert-text-button "github.com/zayfen/rivenemacs"
                            'action (lambda (_) (browse-url "https://github.com/zayfen/rivenemacs"))
                            'help-echo "Visit RivenEmacs repo"
                            'follow-link t)
        (insert-char ?\n)

        (goto-char 0)
        (read-only-mode t)

        (local-set-key (kbd "<escape>") (lambda () (interactive) (rivenemacs-splash-kill)))
        (local-set-key (kbd "q") (lambda () (interactive) (rivenemacs-splash-kill)))
        (local-set-key (kbd "<mouse-1>") 'mouse-set-point)
        (local-set-key (kbd "<mouse-2>") 'operate-this-button)

        (display-buffer-same-window buffer nil)))))

(defun rivenemacs-splash-kill ()
  (when (get-buffer rivenemacs-splash-buffer-name)
    (kill-buffer rivenemacs-splash-buffer-name)))

;; Display splash screen
(rivenemacs-splash)

;; Close splash screen automatically after Emacs gets loaded
(add-hook
 'emacs-startup-hook
 (defun +rivenemacs-splash--kill-h ()
   (run-at-time 0.5 nil #'rivenemacs-splash-kill)))


(provide 're-splash)
