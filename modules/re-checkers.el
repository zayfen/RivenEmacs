;;; re-checkers.el --- Syntax checking -*- lexical-binding: t; -*-


;; (use-package flycheck-pos-tip
;;   :after flycheck
;;   :hook (global-flycheck-mode . flycheck-pos-tip-mode))

(use-package flycheck
  :ensure t
  :if (version<= "24.4" emacs-version)
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
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (setq flycheck-posframe-warning-prefix "\u26a0 ")
  (setq flycheck-posframe-error-prefix "\u26a0 ")
  (setq flycheck-posframe-broder-width 5)
  (setq flycheck-posframe-position 'point-top-left-corner)

  (set-face-attribute 'flycheck-posframe-error-face
                      nil
                      :inherit nil
                      :foreground "red")

  (set-face-attribute 'flycheck-posframe-warning-face
                      nil
                      :foreground "orange")

  (set-face-attribute 'flycheck-posframe-info-face
                      nil
                      :foreground "blue")

  (set-face-attribute 'flycheck-posframe-border-face
                      nil
                      :foreground "#dc752f")

  )

(use-package flycheck-rust
  :ensure t
  :after flycheck
  :commands flycheck-rust-setup
  :hook ((rust-mode . flycheck-rust-setup)
         (rust-ts-mode . flycheck-rust-setup)))

(provide 're-checkers)
