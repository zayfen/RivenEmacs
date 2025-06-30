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
    (condition-case err
        (let* ((current-point (point))
               (line-start (car nb/current-line))
               (line-end (cdr nb/current-line))
               (start (max current-point line-start))
               (end (min limit line-end)))
          (when (and (< start end)
                     (numberp start)
                     (numberp end)
                     (<= start end)
                     (<= start (point-max))
                     (<= end (point-max)))
            (remove-text-properties start end
                                    '(invisible t display "" composition ""))
            (goto-char limit)
            t))
      (error 
       (message "nb/unhide-current-line error: %s" err)
       (goto-char limit)
       t)))

  ;; FIXED: Handle end-of-buffer case where (line-beginning-position 2) returns nil
  (defun nb/refontify-on-linemove ()
    "Post-command-hook"
    (condition-case err
        (let* ((start (line-beginning-position))
               (end (or (line-beginning-position 2) (point-max)))  ;; Handle nil case
               (needs-update (and start 
                                  end
                                  (not (equal start (car nb/current-line))))))
          (when (and start end (numberp start) (numberp end))
            (setq nb/current-line (cons start end)))
          (when (and needs-update 
                     (not (minibufferp))
                     (buffer-live-p (current-buffer))
                     (numberp start)
                     (numberp end))
            (condition-case font-lock-err
                (font-lock-fontify-block 3)
              (error (message "Font-lock error: %s" font-lock-err)))))
      (error (message "nb/refontify-on-linemove error: %s" err))))

  (defun nb/markdown-unhighlight ()
    "Enable markdown concealing"
    (interactive)
    (condition-case err
        (progn
          (markdown-toggle-markup-hiding 'toggle)
          ;; Initialize current line properly
          (setq nb/current-line (cons (line-beginning-position) 
                                      (or (line-beginning-position 2) (point-max))))
          (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
          (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))
      (error (message "nb/markdown-unhighlight error: %s" err))))
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
