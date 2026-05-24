;;; keybindings-commands.el --- Commands used by keybinding specs -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused interactive helpers for structured keybinding specs.

;;; Code:

(require 'project)
(require 'subr-x)

(declare-function consult-ripgrep-ex "init-helper")
(declare-function symbol-overlay-put "symbol-overlay")

(defun riven/keybindings-current-file ()
  "Return the current buffer file name, or nil for non-file buffers."
  (buffer-file-name (buffer-base-buffer)))

(defun riven/keybindings-copy-current-file-path ()
  "Copy the current file path to the kill ring and echo it.
Signal a user error when the current buffer is not visiting a file."
  (interactive)
  (let ((file (riven/keybindings-current-file)))
    (unless file
      (user-error "Current buffer is not visiting a file"))
    (let ((path (file-truename file)))
      (kill-new path)
      (message "%s" path))))

(defun riven/keybindings-rename-current-file-or-buffer (new-name)
  "Rename the current file-backed buffer or rename a non-file buffer to NEW-NAME."
  (interactive
   (list
    (read-string
     (if (riven/keybindings-current-file)
         "Rename file to: "
       "Rename buffer to: ")
     (if-let* ((file (riven/keybindings-current-file)))
         (file-name-nondirectory file)
       (buffer-name)))))
  (if-let* ((file (riven/keybindings-current-file)))
      (let* ((target (expand-file-name new-name (file-name-directory file)))
             (target-dir (file-name-directory target)))
        (when (file-exists-p target)
          (user-error "Target already exists: %s" (abbreviate-file-name target)))
        (unless (file-directory-p target-dir)
          (make-directory target-dir t))
        (rename-file file target)
        (set-visited-file-name target t t)
        (message "Renamed to %s" (abbreviate-file-name target)))
    (rename-buffer new-name t)
    (message "Renamed buffer to %s" new-name)))

(defun riven/keybindings-delete-current-file-or-buffer ()
  "Delete the current file and kill its buffer, or kill a non-file buffer."
  (interactive)
  (if-let* ((file (riven/keybindings-current-file)))
      (when (yes-or-no-p (format "Delete file %s? " (abbreviate-file-name file)))
        (delete-file file)
        (kill-buffer (current-buffer))
        (message "Deleted %s" (abbreviate-file-name file)))
    (kill-buffer (current-buffer))))

(defun riven/keybindings-project-root ()
  "Return current project root, or `default-directory' when outside a project."
  (if-let* ((project (project-current nil)))
      (project-root project)
    default-directory))

(defun riven/keybindings-project-dired ()
  "Open Dired at the current project root or `default-directory'."
  (interactive)
  (dired (riven/keybindings-project-root)))

(defun riven/keybindings-symbol-overlay-dispatch ()
  "Run `symbol-overlay-put' when available, otherwise search symbol at point."
  (interactive)
  (cond
   ((fboundp 'symbol-overlay-put)
    (call-interactively #'symbol-overlay-put))
   ((fboundp 'consult-ripgrep-ex)
    (call-interactively #'consult-ripgrep-ex))
   (t
    (user-error "No symbol highlight/search command is available"))))

(provide 'keybindings-commands)
;;; keybindings-commands.el ends here
