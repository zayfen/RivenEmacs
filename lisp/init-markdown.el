;;; -*- coding: utf-8; lexical-binding: t -*-

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init 
  (setq markdown-command "pandoc")  ; More powerful than multimarkdown
  :hook
  (markdown-mode . (lambda ()
                     (nb/markdown-unhighlight)
                     (variable-pitch-mode 1)  ; Better prose readability
                     (visual-line-mode 1)))   ; Soft word wrapping
  :config
  (defvar nb/current-line '(0 . 0)
    "(start . end) of current line in current buffer")
  (make-variable-buffer-local 'nb/current-line)

  (defun nb/unhide-current-line (limit)
    "Font-lock function"
    (let ((start (max (point) (car nb/current-line)))
          (end (min limit (cdr nb/current-line))))
      (when (< start end)
        (remove-text-properties start end
                                '(invisible t display "" composition ""))
        (goto-char limit)
        t)))

  (defun nb/refontify-on-linemove ()
    "Post-command-hook"
    (let* ((start (line-beginnin:g-position))
           (end (line-beginning-position 2))
           (needs-update (not (equal start (car nb/current-line)))))
      (setq nb/current-line (cons start end))
      (when needs-update
        (font-lock-fontify-block 3))))

  (defun nb/markdown-unhighlight ()
    "Enable markdown concealling"
    (interactive)
    (markdown-toggle-markup-hiding 'toggle)
    (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
    (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))
  ;; Enhanced color scheme with better contrast and modern aesthetics
  (custom-set-faces
   '(markdown-header-delimiter-face ((t (:foreground "#5e81ac" :height 0.9))))
   '(markdown-header-face-1 ((t (:height 1.8 :foreground "#8fbcbb" :weight extra-bold :inherit markdown-header-face))))
   '(markdown-header-face-2 ((t (:height 1.6 :foreground "#88c0d0" :weight extra-bold :inherit markdown-header-face))))
   '(markdown-header-face-3 ((t (:height 1.4 :foreground "#81a1c1" :weight bold :inherit markdown-header-face))))
   '(markdown-header-face-4 ((t (:height 1.2 :foreground "#5e81ac" :weight bold :inherit markdown-header-face))))
   '(markdown-header-face-5 ((t (:height 1.1 :foreground "#d08770" :weight semi-bold :inherit markdown-header-face))))
   '(markdown-header-face-6 ((t (:height 1.05 :foreground "#a3be8c" :weight semi-bold :inherit markdown-header-face))))
   '(markdown-code-face ((t (:family "LigaSFMonoNerdFont" :background "#2e3440" :foreground "#d8dee9"))))
   '(markdown-inline-code-face ((t (:inherit 'markdown-code-face :height 0.95)))))

  ;; Additional quality-of-life settings
  (setq markdown-fontify-code-blocks-natively t
        markdown-hide-markup t
        markdown-list-indent-width 2
        markdown-gfm-additional-languages '("bash" "python" "emacs-lisp")
        markdown-enable-math t)))

(provide 'init-markdown)
;;; init-markdown.el ends here
