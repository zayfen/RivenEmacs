;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-web.el --- web config

(defun riven/web-mode-setup ()
  "Common setup for `web-mode' buffers."
  (emmet-mode 1)
  (local-unset-key (kbd "C-c C-l")))


(use-package emmet-mode
  :ensure t
  :commands (emmet-mode))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.ejs\\'")
  :hook (web-mode . riven/web-mode-setup)
  :custom
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-quoting nil)
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-style-padding 0)
  (web-mode-script-padding 0))

(use-package vue-ts-mode
  :vc (:url "https://github.com/8uff3r/vue-ts-mode" :branch "main")
  :mode "\\.vue\\'"
  :hook (vue-ts-mode . eglot-ensure))


(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-ts-mode))

;; config indent
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq js-switch-indent-offset 2)
(setq typescript-ts-mode-indent-offset 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; React/JSX/TSX Tree-sitter Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)

(defun riv/treesit-get-ancestor-matching (node predicate)
  "Traverse up from NODE, return the first ancestor that satisfies PREDICATE."
  (let ((current-node node))
    (while (and current-node (not (funcall predicate current-node)))
      (setq current-node (treesit-node-parent current-node)))
    current-node))

(defun riv/treesit-jsx-goto-matching-tag ()
  "Navigate between the opening and closing tags of a JSX element."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         ;; Find the innermost jsx_element containing the point.
         (element-node (riv/treesit-get-ancestor-matching
                        node
                        (lambda (n) (string-equal (treesit-node-type n) "jsx_element")))))
    (when element-node
      (let* ((children (treesit-node-children element-node))
             (opening-tag (car children))
             (closing-tag (car (last children))))
        ;; Ensure we have valid opening and closing tags before proceeding.
        (when (and opening-tag
                   (string-equal (treesit-node-type opening-tag) "jsx_opening_element")
                   closing-tag
                   (string-equal (treesit-node-type closing-tag) "jsx_closing_element"))
          (let ((point-pos (point))
                (closing-tag-start (treesit-node-start closing-tag)))
            ;; If point is before the start of the closing tag, jump to it.
            ;; Otherwise, jump to the opening tag. This handles being in the
            ;; opening tag, between tags, or on the closing tag itself.
            (if (< point-pos closing-tag-start)
                (goto-char closing-tag-start)
              (goto-char (treesit-node-start opening-tag)))))))))

(defun riv/treesit-jsx-delete-tag ()
  "Delete the entire JSX element under the cursor.
This includes the opening tag, closing tag, and all content within.
Relies on `tree-sitter` to find the boundaries of the jsx_element."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (element-node (riv/treesit-get-ancestor-matching node (lambda (p) (string-equal (treesit-node-type p) "jsx_element")))))
    (when element-node
      (delete-region (treesit-node-start element-node) (treesit-node-end element-node)))))

(defun riv/treesit-jsx-rename-tag ()
  "Rename the JSX tag under the cursor using tree-sitter."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (element-node (riv/treesit-get-ancestor-matching
                        node
                        (lambda (n) (or (string-equal (treesit-node-type n) "jsx_element")
                                        (string-equal (treesit-node-type n) "jsx_self_closing_element"))))))
    (when element-node
      (let* ((is-self-closing (string-equal (treesit-node-type element-node) "jsx_self_closing_element"))
             (opening-tag-node (if is-self-closing element-node (car (treesit-node-children element-node))))
             (name-node (treesit-node-child-by-field-name opening-tag-node "name")))
        (when name-node
          (let* ((current-name (buffer-substring-no-properties (treesit-node-start name-node) (treesit-node-end name-node)))
                 (new-name (read-from-minibuffer (format "New tag name (was %s): " current-name) current-name))
                 ;; Store all positions BEFORE modifying the buffer to avoid outdated nodes.
                 (name-start (treesit-node-start name-node))
                 (name-end (treesit-node-end name-node))
                 (closing-tag-node (unless is-self-closing (car (last (treesit-node-children element-node)))))
                 (close-name-node (when closing-tag-node (treesit-node-child-by-field-name closing-tag-node "name")))
                 (close-name-start (when close-name-node (treesit-node-start close-name-node)))
                 (close-name-end (when close-name-node (treesit-node-end close-name-node))))
            (when (and new-name (> (length new-name) 0) (not (string-equal new-name current-name)))
              (undo-boundary)
              ;; Modify from the end of the buffer to the start to keep positions valid.
              (when close-name-start
                (delete-region close-name-start close-name-end)
                (goto-char close-name-start)
                (insert new-name))
              (delete-region name-start name-end)
              (goto-char name-start)
              (insert new-name))))))))

(defun riv/in-jsx-context-p ()
  "Return non-nil if the point is inside a JSX element but not in a JS expression."
  (let ((node (treesit-node-at (point))))
    (and (riv/treesit-get-ancestor-matching node (lambda (n) (string-match-p "^jsx_" (treesit-node-type n))))
         (not (riv/treesit-get-ancestor-matching node (lambda (n) (string-equal (treesit-node-type n) "jsx_expression")))))))

(defun riv/jsx-comment-dwim (arg)
  "Context-aware comment command for JSX.
Uses {/* ... */} inside JSX tags and `comment-dwim` elsewhere."
  (interactive "P")
  (if (riv/in-jsx-context-p)
      ;; In JSX context, temporarily override all relevant comment variables
      ;; and then call the standard `comment-dwim` command. This is the
      ;; most robust way to handle all cases (region, no region, etc.).
      (let* ((cs "{/* ")
             (ce " */}")
             (css (concat "\\s-*" (regexp-quote cs))))
        (let ((comment-start cs)
              (comment-end ce)
              (comment-start-skip css))
          (comment-dwim arg)))
    ;; Outside JSX, use the default behavior.
    (comment-dwim arg)))

(defun my-react-mode-hook ()
  "Hook for React modes (tsx, jsx) to add keybindings."
  ;; Helpers for react under `C-c C-e` prefix
  ;; C-c C-e r -> rename tag (tree-sitter)
  ;; C-c C-e j -> jump to matching tag (tree-sitter)
  ;; C-c C-e d -> delete tag pair (tree-sitter)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'riv/treesit-jsx-rename-tag)
    (define-key map (kbd "j") 'riv/treesit-jsx-goto-matching-tag)
    (define-key map (kbd "d") 'riv/treesit-jsx-delete-tag)
    (local-set-key (kbd "C-c C-e") map))
  ;; Override comment command for JSX context
  (local-set-key (kbd "M-;") 'riv/jsx-comment-dwim))

; Add the hook to tsx and jsx modes.
(add-hook 'tsx-ts-mode-hook 'my-react-mode-hook)
(add-hook 'js-ts-mode-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (string-match-p "\\.jsx\\\'" (buffer-file-name)))
              (my-react-mode-hook))))


(provide 'init-web)
