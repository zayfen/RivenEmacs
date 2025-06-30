;; -*- lexical-binding: t; -*-

(use-package markdown-mode
  :hook
  (markdown-mode . nb/markdown-unhighlight)
  :custom
  (markdown-header-scaling t)
  (markdown-hide-urls t)
  (markdown-fontify-code-blocks-natively t)
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

 (defun nb/markdown-unhighlight ()
   "Enable markdown concealling"
   (interactive)
   (markdown-toggle-markup-hiding 'toggle)
   (font-lock-add-keywords nil '((nb/unhide-current-line)) t))

  :custom-face
  (markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
  (markdown-header-face-1 ((t (:height 1.6  :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-2 ((t (:height 1.4  :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-3 ((t (:height 1.2  :foreground "#D08770" :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-4 ((t (:height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face))))
  (markdown-header-face-5 ((t (:height 1.1  :foreground "#b48ead" :weight bold :inherit markdown-header-face))))
  (markdown-header-face-6 ((t (:height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))))
  :hook
  (markdown-mode . abbrev-mode))

;; Use keybindings
(use-package grip-mode
  :ensure t
  :config (setq grip-command 'auto) ;; auto, grip, go-grip or mdopen
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))

(provide 'init-markdown)
;;; init-markdown.el ends here
