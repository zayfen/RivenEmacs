;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-terminal.el --- Terminal

(use-package eat
  :vc (:url "https://codeberg.org/akib/emacs-eat")
  :commands (eat eat-line-mode eat-semi-char-mode)
  :hook (eat-exec . (lambda (_) (eat-semi-char-mode))))


(add-hook 'eshell-load-hook #'eat-eshell-mode)

;; (use-package vterm
;;   :ensure t
;;   :custom
;;   (vterm-shell "/usr/bin/zsh")  ; Set your default shell
;;   (vterm-max-scrollback 10000)  ; Increase scrollback buffer
;;   (vterm-kill-buffer-on-exit t)  ; Auto-kill buffer on exit
;;   :bind
;;   (:map vterm-mode-map
;;         ("C-c C-c" . vterm-send-C-c)  ; Send C-c to terminal
;;         ("C-c C-y" . vterm-yank)  ; Yank from kill-ring
;;         ("C-c C-t" . vterm-copy-mode))  ; Toggle copy mode

;;   :config
;;   ;; Enable directory tracking
;;   (add-hook 'vterm-mode-hook #'vterm-set-title)

;;   ;; Optional: Auto-close when process finishes
;;   (setq vterm-kill-buffer-on-exit t)
;; )

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
