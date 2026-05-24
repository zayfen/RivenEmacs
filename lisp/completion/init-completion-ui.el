;; -*- coding: utf-8; lexical-binding: t -*-

;;; init-completion-ui.el --- In-buffer completion UI

;;; Code:

(require 'init-minibuffer)

(defun riven/corfu-nerd-icons-formatter (metadata)
  "Return a Corfu margin formatter using nerd-icons with a fallback icon.
METADATA is passed through to `nerd-icons-corfu-formatter' when kind data exists."
  (if (and (fboundp 'nerd-icons-corfu-formatter)
           (plist-get completion-extra-properties :company-kind))
      (nerd-icons-corfu-formatter metadata)
    (let ((fallback (when (fboundp 'nerd-icons-codicon)
                      (nerd-icons-codicon "nf-cod-code" :face 'font-lock-warning-face)))
          (space (propertize " " 'display '(space :width 1))))
      (when fallback
        (lambda (_cand)
          (concat " " fallback space))))))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'riven/corfu-nerd-icons-formatter))

(defun riven/corfu-enable-nerd-icons ()
  "Load `nerd-icons-corfu' and enable its formatter for Corfu margins."
  (if (require 'nerd-icons-corfu nil t)
      (add-to-list 'corfu-margin-formatters #'riven/corfu-nerd-icons-formatter)
    (message "[corfu] nerd-icons-corfu extension unavailable")))

(defun riven/corfu-insert-index-from-key ()
  "Insert Corfu candidate selected by `M-0..M-9' key without pressing RET.
`M-1'..`M-9' insert candidate 1..9 in the current page. `M-0' inserts 10."
  (interactive)
  (let* ((base-key (event-basic-type last-command-event))
         (digit (- base-key ?0))
         (slot (if (zerop digit) 10 digit))
         (target (+ corfu--scroll (1- slot)))
         (page-end (+ corfu--scroll corfu-count)))
    (if (and (>= target 0) (< target corfu--total) (< target page-end))
        (progn
          (corfu--goto target)
          (corfu-insert))
      (message "[corfu] index %d out of current page range" slot))))

(use-package corfu
  :ensure t
  :demand t
  :custom
  (tab-always-indent 'complete)
  (corfu-on-exact-match 'insert)
  (corfu-auto t)
  (corfu-auto-delay 0.12)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-count 9)
  (corfu-preselect 'first)
  (corfu-preview-current t)
  (corfu-min-width 48)
  (corfu-max-width 120)
  (corfu-scroll-margin 2)
  :bind (:map corfu-map
              ("M-0" . riven/corfu-insert-index-from-key)
              ("M-1" . riven/corfu-insert-index-from-key)
              ("M-2" . riven/corfu-insert-index-from-key)
              ("M-3" . riven/corfu-insert-index-from-key)
              ("M-4" . riven/corfu-insert-index-from-key)
              ("M-5" . riven/corfu-insert-index-from-key)
              ("M-6" . riven/corfu-insert-index-from-key)
              ("M-7" . riven/corfu-insert-index-from-key)
              ("M-8" . riven/corfu-insert-index-from-key)
              ("M-9" . riven/corfu-insert-index-from-key)
              ("M-P" . corfu-scroll-down)
              ("M-N" . corfu-scroll-up))
  :config
  (global-corfu-mode 1)
  (if (require 'corfu-indexed nil t)
      (progn
        (setq corfu-indexed-start 1)
        (corfu-indexed-mode 1))
    (message "[corfu] corfu-indexed extension unavailable"))
  (riven/corfu-enable-nerd-icons))

(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :bind (:map corfu-map
              ("M-h" . corfu-popupinfo-documentation))
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-max-width 100)
  (corfu-popupinfo-max-height 14)
  :config
  (corfu-popupinfo-mode 1))

(use-package cape
  :ensure t
  :config
  (dolist (capf '(cape-file cape-dabbrev cape-keyword cape-symbol))
    (setq completion-at-point-functions (delq capf completion-at-point-functions))
    (when (fboundp capf)
      (add-hook 'completion-at-point-functions capf t)))
  (with-eval-after-load 'eglot
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (when (bound-and-true-p eglot--managed-mode)
                  (setq-local completion-at-point-functions
                              (list #'eglot-completion-at-point
                                    #'cape-dabbrev
                                    #'cape-file)))))
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))

(provide 'init-completion-ui)
;;; init-completion-ui.el ends here
