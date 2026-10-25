;; -*- coding: utf-8; lexical-binding: t -*-

;;; init-completion-ui.el --- In-buffer completion UI

;;; Code:

(require 'init-minibuffer)

(declare-function corfu-next "corfu" (&optional n))
(declare-function corfu-previous "corfu" (&optional n))
(declare-function corfu-popupinfo-scroll-up "corfu-popupinfo" (&optional n))
(declare-function corfu-popupinfo-scroll-down "corfu-popupinfo" (&optional n))
(declare-function corfu-popupinfo--visible-p "corfu-popupinfo" (&optional frame))

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
  "Insert the Corfu candidate selected by `M-1'..`M-9' without pressing RET.
`M-1'..`M-9' insert the 1st..9th candidate of the current page.
`M-0' would select a 10th candidate, but `corfu-count' is 9 by default, so
no page has a 10th slot; `M-0' only reports that instead of inserting."
  (interactive)
  (let* ((base-key (event-basic-type last-command-event))
         (digit (- base-key ?0))
         (slot (if (zerop digit) 10 digit))
         (target (+ corfu--scroll (1- slot)))
         (page-end (+ corfu--scroll corfu-count)))
    (cond
     ;; The current page ends before SLOT (with `corfu-count' 9 there is no
     ;; 10th slot), so report it instead of calling `corfu--goto' out of range.
     ((>= (1- slot) corfu-count)
      (message "[corfu] no %dth candidate on the current page (corfu-count=%d)"
               slot corfu-count))
     ((and (>= target 0) (< target corfu--total) (< target page-end))
      (corfu--goto target)
      (corfu-insert))
     (t
      (message "[corfu] index %d out of current page range" slot)))))

(defun riven/corfu-doc-visible-p ()
  "Return non-nil when Corfu's documentation popup is visible."
  (and (fboundp 'corfu-popupinfo--visible-p)
       (corfu-popupinfo--visible-p)))

(defun riven/corfu-scroll-doc-forward (&optional n)
  "Scroll Corfu's documentation popup forward, or select the next candidate.
When the documentation popup is visible, scroll it by N lines (a whole
page when N is nil).  Otherwise keep Corfu's usual `M-n' meaning and move
to the next candidate, so the key still works for candidates without
documentation."
  (interactive "P")
  (if (riven/corfu-doc-visible-p)
      (corfu-popupinfo-scroll-up n)
    (corfu-next n)))

(defun riven/corfu-scroll-doc-backward (&optional n)
  "Scroll Corfu's documentation popup backward, or select the previous candidate.
The backward counterpart of `riven/corfu-scroll-doc-forward'."
  (interactive "P")
  (if (riven/corfu-doc-visible-p)
      (corfu-popupinfo-scroll-down n)
    (corfu-previous n)))

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
              ;; C-n/C-p keep switching candidates; M-n/M-p scroll the
              ;; documentation popup when it is visible.
              ("M-n" . riven/corfu-scroll-doc-forward)
              ("M-p" . riven/corfu-scroll-doc-backward)
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
  ;; Load it eagerly: otherwise `:after' only records a dependency, the
  ;; global minor mode below never runs, and waiting/`M-h' shows no docs.
  :demand t
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
  :demand t ; load now, otherwise `use-package-always-defer' skips :config
  :config
  (dolist (capf '(cape-file cape-dabbrev cape-keyword))
    (setq completion-at-point-functions (delq capf completion-at-point-functions))
    (when (fboundp capf)
      ;; Add to the global default so every buffer gets these CAPFs, not only
      ;; the buffer that was current when this file was loaded.
      (add-hook 'completion-at-point-functions capf)))
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
