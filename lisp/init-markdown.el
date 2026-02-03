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
(use-package markdown-mode
  :after general
  :config
  ;; Create a definer for markdown live commands
  (general-create-definer markdown-live-def
    :prefix "C-c l"
    :keymaps 'markdown-mode-map)
  
  ;; Define markdown live command groups
  (markdown-live-def
    "" '(:ignore t :wk "Markdown Live")
    ;; Text formatting
    "b" '(markdown-insert-bold :wk "Bold")
    "i" '(markdown-insert-italic :wk "Italic")
    "c" '(markdown-insert-code :wk "Inline Code")
    "s" '(markdown-insert-strike-through :wk "Strikethrough")
    ;; Headers
    "h" '(:ignore t :wk "Headers")
    "h1" '(markdown-insert-header-1 :wk "Header 1")
    "h2" '(markdown-insert-header-2 :wk "Header 2") 
    "h3" '(markdown-insert-header-3 :wk "Header 3")
    "h4" '(markdown-insert-header-4 :wk "Header 4")
    "h5" '(markdown-insert-header-5 :wk "Header 5")
    "h6" '(markdown-insert-header-6 :wk "Header 6")
    "ha" '(markdown-insert-header-dwim :wk "Auto Header")
    ;; Lists
    "l" '(:ignore t :wk "Lists")
    "lu" '(markdown-insert-list-item :wk "List Item")
    "lo" '(markdown-insert-ordered-list-item :wk "Ordered List")
    "lt" '(markdown-insert-gfm-checkbox :wk "Task List")
    "li" '(markdown-insert-list-item-at-level :wk "List at Level")
    ;; Links and images
    "L" '(:ignore t :wk "Links")
    "Ll" '(markdown-insert-link :wk "Insert Link")
    "Lu" '(markdown-insert-link-dwim :wk "Smart Link")
    "Li" '(markdown-insert-image :wk "Insert Image")
    "Lw" '(markdown-insert-wiki-link :wk "Wiki Link")
    "Lr" '(markdown-insert-reference-link :wk "Ref Link")
    ;; Code blocks
    "C" '(:ignore t :wk "Code")
    "Cb" '(markdown-insert-gfm-code-block :wk "Code Block")
    "Ci" '(markdown-insert-code :wk "Inline Code")
    "Cf" '(markdown-insert-code-block-with-fence :wk "Fenced Code")
    "Cl" '(markdown-insert-gfm-code-block-with-lang :wk "Code with Lang")
    ;; Tables
    "t" '(:ignore t :wk "Tables")
    "tt" '(markdown-insert-table :wk "Insert Table")
    "ta" '(markdown-table-align :wk "Align Table")
    "tr" '(markdown-table-insert-row :wk "Insert Row")
    "tc" '(markdown-table-insert-column :wk "Insert Column")
    "td" '(markdown-table-delete-row :wk "Delete Row")
    "tD" '(markdown-table-delete-column :wk "Delete Column")
    "tm" '(markdown-table-move-row :wk "Move Row")
    "tM" '(markdown-table-move-column :wk "Move Column")
    ;; Horizontal rules and breaks
    "-" '(markdown-insert-hr :wk "Horizontal Rule")
    "_" '(markdown-insert-line-break :wk "Line Break")
    ;; Preview and export
    "p" '(:ignore t :wk "Preview/Export")
    "pp" '(grip-mode :wk "Live Preview")
    "pe" '(markdown-export :wk "Export")
    "po" '(markdown-open :wk "Open Output")
    "pv" '(markdown-live-preview-mode :wk "Live Preview Mode")
    "pw" '(markdown-export-and-preview :wk "Export & Preview")
    ;; Movement and navigation
    "n" '(:ignore t :wk "Navigation")
    "np" '(markdown-previous-link :wk "Prev Link")
    "nn" '(markdown-next-link :wk "Next Link")
    "nf" '(markdown-follow-link-at-point :wk "Follow Link")
    "nb" '(markdown-back-to-heading :wk "Back to Heading")
    "nf" '(markdown-next-heading :wk "Next Heading")
    "np" '(markdown-previous-heading :wk "Prev Heading")
    ;; Toggles and settings
    "T" '(:ignore t :wk "Toggles")
    "Tw" '(markdown-toggle-wiki-links :wk "Wiki Links")
    "Tl" '(markdown-toggle-url-hiding :wk "Hide URLs")
    "Ti" '(markdown-toggle-inline-images :wk "Inline Images")
    "Tm" '(markdown-toggle-markup-hiding :wk "Hide Markup")
    "Tf" '(markdown-fontify-buffer-wiki-links :wk "Fontify Wiki"))
  
  ;; Additional useful functions
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
      (markdown-insert-gfm-code-block lang))))

(provide 'init-markdown)
;;; init-markdown.el ends here
