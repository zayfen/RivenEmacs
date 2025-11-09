;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-terminal.el --- Terminal

(use-package eat
  :vc (:url "https://codeberg.org/akib/emacs-eat")
  :commands (eat eat-line-mode eat-semi-char-mode)
  :hook (eat-exec . (lambda (_) (eat-semi-char-mode))))


(add-hook 'eshell-load-hook #'eat-eshell-mode)

(use-package vterm
  :custom
  (vterm-shell (cond
                 ((file-exists-p "/opt/homebrew/bin/zsh") "/opt/homebrew/bin/zsh")
                 ((file-exists-p "/usr/local/bin/zsh") "/usr/local/bin/zsh")
                 ((file-exists-p "/usr/bin/zsh") "/usr/bin/zsh")
                 ((file-exists-p "/bin/bash") "/bin/bash")
                 ((file-exists-p "/bin/sh") "/bin/sh")
                 (t "zsh")))  ; Set your default shell (with fallback)
  (vterm-max-scrollback 10000)  ; Increase scrollback buffer
  (vterm-kill-buffer-on-exit t)  ; Auto-kill buffer on exit
  ;; :bind
  ;; (:map vterm-mode-map
  ;;       ("C-c C-c" . vterm-send-C-c)  ; Send C-c to terminal
  ;;       ("C-c C-y" . vterm-yank)  ; Yank from kill-ring
  ;;       ("C-c C-t" . vterm-copy-mode))  ; Toggle copy mode
)

;; ;; Optional: vterm-toggle package for better toggling
;; (use-package vterm-toggle
;;   :ensure t
;;   :after vterm
;;   :custom
;;   (vterm-toggle-scope 'project)  ; 'project, 'frame, or 'nil
;;   (vterm-toggle-fullscreen-p nil)
;;   (vterm-toggle-reset-window-configration-after-exit t))

(provide 'init-terminal)
;;; init-terminal.el ends here.
