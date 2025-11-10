;;; code-review.el --- Code review functionality for Emacs -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'json)

(defgroup code-review nil
  "Code review functionality for Emacs."
  :group 'tools)

(defcustom code-review-comments-file "~/.emacs.d/code-review-comments.json"
  "File to store code review comments."
  :type 'string
  :group 'code-review)

(defcustom code-review-auto-save t
  "Whether to auto-save comments."
  :type 'boolean
  :group 'code-review)

(defvar code-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r a") 'code-review-approve)
    (define-key map (kbd "C-c C-r r") 'code-review-reject)
    (define-key map (kbd "C-c C-r c") 'code-review-add-comment)
    (define-key map (kbd "C-c C-r n") 'code-review-next-comment)
    (define-key map (kbd "C-c C-r p") 'code-review-previous-comment)
    (define-key map (kbd "C-c C-r s") 'code-review-show-comments)
    (define-key map (kbd "C-c C-r d") 'code-review-delete-comment)
    map)
  "Keymap for code review mode.")

(define-minor-mode code-review-mode
  "Minor mode for code review."
  :lighter " Review"
  :keymap code-review-mode-map
  :group 'code-review)

(defvar code-review-comments nil
  "List of review comments for current buffer.")

(defvar code-review-overlay-alist nil
  "Alist of (overlay . comment) pairs.")

(defconst code-review-approved-face
  '(:background "#e6ffec" :foreground "#28a745")
  "Face for approved code.")

(defconst code-review-rejected-face
  '(:background "#ffeef0" :foreground "#d73a49")
  "Face for rejected code.")

(defconst code-review-comment-face
  '(:background "#fff5b1" :underline t)
  "Face for commented code.")

;; Initialize the package
(defun code-review-initialize ()
  "Initialize code review system."
  (make-directory (file-name-directory code-review-comments-file) t)
  (code-review-load-comments))

;; Save and load comments
(defun code-review-save-comments ()
  "Save review comments to file."
  (when code-review-auto-save
    (let ((comments-data (code-review-serialize-comments)))
      (with-temp-file code-review-comments-file
        (insert (json-encode comments-data))))))

(defun code-review-load-comments ()
  "Load review comments from file."
  (when (file-exists-p code-review-comments-file)
    (with-temp-buffer
      (insert-file-contents code-review-comments-file)
      (let ((data (json-read)))
        (setq code-review-comments data)))))

(defun code-review-serialize-comments ()
  "Convert comments to serializable format."
  (cl-loop for comment in code-review-comments
           collect (list :file (buffer-file-name)
                        :line (plist-get comment :line)
                        :type (plist-get comment :type)
                        :text (plist-get comment :text)
                        :timestamp (plist-get comment :timestamp))))

(defun code-review-deserialize-comments (data)
  "Convert serialized data back to comments."
  (cl-loop for item in data
           collect (list :file (plist-get item :file)
                        :line (plist-get item :line)
                        :type (plist-get item :type)
                        :text (plist-get item :text)
                        :timestamp (plist-get item :timestamp))))

;; Core review functions
(defun code-review-add-comment (line type text)
  "Add a comment at LINE with TYPE and TEXT."
  (interactive
   (list (line-number-at-pos)
         (completing-read "Comment type: " '("approve" "reject" "comment"))
         (read-string "Comment: ")))
  (let ((comment (list :file (buffer-file-name)
                      :line line
                      :type type
                      :text text
                      :timestamp (current-time-string))))
    (push comment code-review-comments)
    (code-review-save-comments)
    (code-review-highlight-line line type)
    (message "Added %s comment at line %d" type line)))

(defun code-review-approve (&optional line)
  "Approve current line or LINE."
  (interactive)
  (let ((current-line (or line (line-number-at-pos))))
    (code-review-add-comment current-line "approve" "Approved")
    (message "Line %d approved" current-line)))

(defun code-review-reject (&optional line)
  "Reject current line or LINE."
  (interactive)
  (let ((current-line (or line (line-number-at-pos))))
    (code-review-add-comment current-line "reject"
                            (read-string "Rejection reason: "))
    (message "Line %d rejected" current-line)))

(defun code-review-add-comment-interactively ()
  "Add a general comment to current line."
  (interactive)
  (let ((line (line-number-at-pos))
        (comment-text (read-string "Comment: ")))
    (code-review-add-comment line "comment" comment-text)))

;; Highlighting functions
(defun code-review-highlight-line (line type)
  "Highlight LINE with TYPE face."
  (save-excursion
    (goto-line line)
    (let ((beg (line-beginning-position))
          (end (line-end-position))
          (face (cond ((string= type "approve") code-review-approved-face)
                     ((string= type "reject") code-review-rejected-face)
                     (t code-review-comment-face))))
      (let ((overlay (make-overlay beg end)))
        (overlay-put overlay 'face face)
        (overlay-put overlay 'evaporate t)
        (push (cons overlay line) code-review-overlay-alist)))))

(defun code-review-clear-highlights ()
  "Clear all review highlights."
  (interactive)
  (dolist (pair code-review-overlay-alist)
    (delete-overlay (car pair)))
  (setq code-review-overlay-alist nil))

(defun code-review-refresh-highlights ()
  "Refresh all highlights based on current comments."
  (code-review-clear-highlights)
  (dolist (comment code-review-comments)
    (when (and (plist-get comment :file)
               (string= (plist-get comment :file) (buffer-file-name)))
      (code-review-highlight-line (plist-get comment :line)
                                 (plist-get comment :type)))))

;; Navigation functions
(defun code-review-next-comment ()
  "Move to next commented line."
  (interactive)
  (let ((current-line (line-number-at-pos))
        (next-line nil))
    (dolist (comment code-review-comments)
      (let ((comment-line (plist-get comment :line)))
        (when (and (> comment-line current-line)
                   (or (not next-line) (< comment-line next-line)))
          (setq next-line comment-line))))
    (if next-line
        (progn
          (goto-line next-line)
          (message "Moved to line %d" next-line))
      (message "No more comments"))))

(defun code-review-previous-comment ()
  "Move to previous commented line."
  (interactive)
  (let ((current-line (line-number-at-pos))
        (prev-line nil))
    (dolist (comment code-review-comments)
      (let ((comment-line (plist-get comment :line)))
        (when (and (< comment-line current-line)
                   (or (not prev-line) (> comment-line prev-line)))
          (setq prev-line comment-line))))
    (if prev-line
        (progn
          (goto-line prev-line)
          (message "Moved to line %d" prev-line))
      (message "No previous comments"))))

;; Display functions
(defun code-review-show-comments ()
  "Show all comments in a separate buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Code Review Comments*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert "Code Review Comments:\n\n")
      (dolist (comment (reverse code-review-comments))
        (insert (format "Line %d [%s]: %s (%s)\n"
                       (plist-get comment :line)
                       (plist-get comment :type)
                       (plist-get comment :text)
                       (plist-get comment :timestamp))))
      (read-only-mode 1)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(defun code-review-delete-comment ()
  "Delete comment at current line."
  (interactive)
  (let ((current-line (line-number-at-pos))
        (deleted nil))
    (setq code-review-comments
          (cl-remove-if (lambda (comment)
                         (when (and (= (plist-get comment :line) current-line)
                                   (string= (plist-get comment :file) (buffer-file-name)))
                           (setq deleted t)
                           t))
                       code-review-comments))
    (when deleted
      (code-review-refresh-highlights)
      (code-review-save-comments)
      (message "Deleted comment at line %d" current-line))))

;; Summary functions
(defun code-review-summary ()
  "Show review summary."
  (interactive)
  (let ((approved 0)
        (rejected 0)
        (commented 0))
    (dolist (comment code-review-comments)
      (cond ((string= (plist-get comment :type) "approve") (cl-incf approved))
            ((string= (plist-get comment :type) "reject") (cl-incf rejected))
            (t (cl-incf commented))))
    (message "Review Summary: %d approved, %d rejected, %d comments"
             approved rejected commented)))

;; Auto-mode setup
(defun code-review-enable-auto-mode ()
  "Enable code review mode automatically for certain files."
  (when (and buffer-file-name
             (member (file-name-extension buffer-file-name)
                     '("c" "cpp" "h" "hpp" "py" "js" "java" "go" "rs")))
    (code-review-mode 1)
    (code-review-refresh-highlights)))

;; Hook functions
(add-hook 'find-file-hook 'code-review-enable-auto-mode)
(add-hook 'kill-buffer-hook 'code-review-save-comments)

;; Initialize on load
(code-review-initialize)

(provide 'code-review)

;;; code-review.el ends here
