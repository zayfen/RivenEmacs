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
  (markdown-strike-through-face ((t (:strike-through t :height 1.0))))
  :hook
  (markdown-mode . abbrev-mode))

;; Grip live preview mode
(use-package grip-mode
  :ensure t
  :after markdown-mode
  :config
  ;; Use go-grip command
  (setq grip-command 'go-grip)          ;auto, grip, go-grip or mdopen
  
  ;; Function to check and install go-grip if needed
  (defun riven/install-grip-if-needed ()
    "Install go-grip if not found in system."
    (interactive)
    (unless (executable-find "go-grip")
      (message "go-grip not found. Installing...")
      (condition-case err
          (if (executable-find "go")
              (shell-command "go install github.com/chrishrb/go-grip@latest")
            (error "No Go found. Please install go-grip manually: go install github.com/chrishrb/go-grip@latest"))
        (if (executable-find "go-grip")
            (message "go-grip installed successfully!")
          (error "Failed to install go-grip: %s" (error-message-string err))))))
  
  ;; Function to enable grip-mode with auto-install
  (defun grip-mode-maybe-enable ()
    "Enable grip-mode, installing go-grip if necessary."
    (when (and (buffer-file-name)
               (string-match-p "\\.md\\'" (buffer-file-name)))
      (unless (executable-find "go-grip")
        (riven/install-grip-if-needed))
      (when (executable-find "go-grip")
        (grip-mode +1))))
  
  ;; Auto-refresh settings
  (setq grip-auto-refresh t)
  (setq grip-update-on-save t)
  (setq grip-preview-use-webkit t)
  
  ;; Custom keybindings for grip-mode
  :bind (:map markdown-mode-map
         ("C-c C-p" . grip-mode))
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))

;; Markdown Live Commands - C-c l
(with-eval-after-load 'markdown-mode
  (defun markdown-insert-header-dwim ()
    "Insert header based on context (DWIM - Do What I Mean)."
    (interactive)
    (if (markdown-at-heading-p)
        (markdown-demote-heading)
      (markdown-insert-header-1)))

  (defun markdown-insert-link-dwim ()
    "Smart link insertion based on context."
    (interactive)
    (if (use-region-p)
        (let ((link-text (buffer-substring-no-properties (region-beginning) (region-end))))
          (delete-region (region-beginning) (region-end))
          (markdown-insert-link nil link-text))
      (markdown-insert-link)))

  (defun markdown-insert-gfm-code-block-with-lang ()
    "Insert GFM code block with language prompt."
    (interactive)
    (let ((lang (read-string "Language: " "python")))
      (markdown-insert-gfm-code-block lang)))

  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") #'markdown-insert-bold)
    (define-key map (kbd "i") #'markdown-insert-italic)
    (define-key map (kbd "c") #'markdown-insert-code)
    (define-key map (kbd "s") #'markdown-insert-strike-through)
    ;; Headers
    (define-key map (kbd "h 1") #'markdown-insert-header-1)
    (define-key map (kbd "h 2") #'markdown-insert-header-2)
    (define-key map (kbd "h 3") #'markdown-insert-header-3)
    (define-key map (kbd "h 4") #'markdown-insert-header-4)
    (define-key map (kbd "h 5") #'markdown-insert-header-5)
    (define-key map (kbd "h 6") #'markdown-insert-header-6)
    (define-key map (kbd "h a") #'markdown-insert-header-dwim)
    ;; Lists
    (define-key map (kbd "l u") #'markdown-insert-list-item)
    (define-key map (kbd "l o") #'markdown-insert-ordered-list-item)
    (define-key map (kbd "l t") #'markdown-insert-gfm-checkbox)
    ;; Links
    (define-key map (kbd "L l") #'markdown-insert-link)
    (define-key map (kbd "L u") #'markdown-insert-link-dwim)
    (define-key map (kbd "L i") #'markdown-insert-image)
    (define-key map (kbd "L w") #'markdown-insert-wiki-link)
    (define-key map (kbd "L r") #'markdown-insert-reference-link)
    ;; Code
    (define-key map (kbd "C b") #'markdown-insert-gfm-code-block)
    (define-key map (kbd "C i") #'markdown-insert-code)
    (define-key map (kbd "C l") #'markdown-insert-gfm-code-block-with-lang)
    ;; Tables
    (define-key map (kbd "t t") #'markdown-insert-table)
    (define-key map (kbd "t a") #'markdown-table-align)
    (define-key map (kbd "t r") #'markdown-table-insert-row)
    (define-key map (kbd "t c") #'markdown-table-insert-column)
    (define-key map (kbd "t d") #'markdown-table-delete-row)
    (define-key map (kbd "t D") #'markdown-table-delete-column)
    ;; Misc
    (define-key map (kbd "-") #'markdown-insert-hr)
    (define-key map (kbd "_") #'markdown-insert-line-break)
    ;; Preview
    (define-key map (kbd "p p") #'grip-mode)
    (define-key map (kbd "p e") #'markdown-export)
    (define-key map (kbd "p o") #'markdown-open)
    (define-key map (kbd "p v") #'markdown-live-preview-mode)
    ;; Navigation
    (define-key map (kbd "n n") #'markdown-next-link)
    (define-key map (kbd "n N") #'markdown-next-heading)
    (define-key map (kbd "n f") #'markdown-follow-link-at-point)
    (define-key map (kbd "n b") #'markdown-back-to-heading)
    (define-key map (kbd "n p") #'markdown-previous-link)
    (define-key map (kbd "n P") #'markdown-previous-heading)
    ;; Toggles
    (define-key map (kbd "T w") #'markdown-toggle-wiki-links)
    (define-key map (kbd "T l") #'markdown-toggle-url-hiding)
    (define-key map (kbd "T i") #'markdown-toggle-inline-images)
    (define-key map (kbd "T m") #'markdown-toggle-markup-hiding)
    (define-key markdown-mode-map (kbd "C-c l") map)))

(provide 'init-markdown)
;;; init-markdown.el ends here
