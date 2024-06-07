;;; re-checkers.el --- Syntax checking -*- lexical-binding: t; -*-

(use-package flycheck
  :straight t
  :commands flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-initialize-packages t
                flycheck-highlighting-mode 'lines)
  (define-key flycheck-mode-map [remap next-error] #'flycheck-next-error)
  (define-key flycheck-mode-map [remap previous-error] #'flycheck-previous-error)

  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled)))


(use-package flycheck-posframe
  :straight t
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :init
  (setq flycheck-posframe-position 'window-bottom-right-corner)
  (setq flycheck-posframe-warning-prefix "\u26a0 ")
  (setq flycheck-posframe-error-prefix "\u26a0 ")
  (flycheck-posframe-configure-pretty-defaults)
  (setq flycheck-posframe-border-width 4)


  (set-face-attribute 'flycheck-posframe-error-face
                      nil
                      :inherit nil
                      :foreground "red")

  (set-face-attribute 'flycheck-posframe-warning-face
                      nil
                      :foreground "yellow")

  (set-face-attribute 'flycheck-posframe-info-face
                      nil
                      :foreground "green")

  (set-face-attribute 'flycheck-posframe-background-face
                      nil
                      :background "#00246B")

  (set-face-attribute 'flycheck-posframe-border-face
                      nil
                      :foreground "#CADCFC")

  )

(use-package flycheck-rust
  :straight t
  :after flycheck
  :commands flycheck-rust-setup
  :hook ((rust-mode . flycheck-rust-setup)
         (rust-ts-mode . flycheck-rust-setup)))

(flycheck-define-checker python-ruff
  "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
  :command ("ruff"
            "check"
            (eval (when buffer-file-name
                    (concat "" buffer-file-name)))
            "-q")
  :standard-input t
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  :modes (python-ts-mode))

;; Use something adapted to your config to add `python-ruff' to `flycheck-checkers'
;; This is an MVP example:
(setq python-ts-mode-hook
      (list (defun my-python-hook ()
              (unless (bound-and-true-p org-src-mode)
                (when (buffer-file-name)
                  (setq-local flycheck-checkers '(python-ruff))
                  (flycheck-mode))))))


;; javascript use oxlint instead of eslint
(when (executable-find "oxlint")
  (setq flycheck-javascript-eslint-executable "oxlint"))


(provide 're-checkers)
