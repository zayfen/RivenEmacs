;; -*- coding: utf-8; lexical-binding: t -*-

(defun get-project-root ()
  (if (fboundp 'projectile-project-root)
    (projectile-project-root)
    (project-root (project-current))))

;; Ripgrep the current word from project root
(defun consult-ripgrep-at-point ()
  (interactive)
  (consult-ripgrep (get-project-root) (thing-at-point 'symbol)))

;; Ripgrep the selected region from project root
(defun consult-ripgrep-region ()
  (interactive)
  (consult-ripgrep (get-project-root) (buffer-substring-no-properties (region-beginning) (region-end))))

;; enhance ripgrep
(defun consult-ripgrep-ex ()
  (interactive)
  (if (use-region-p)
      (consult-ripgrep-region)
    (consult-ripgrep-at-point)))

(defun my-select-inside-quotes ()
  "grab text between double straight quotes on each side of cursor."
  (interactive)
  (let (p1 p2)
    (skip-chars-backward "^[\"\']" (line-beginning-position))
    (setq p1 (point))
    (skip-chars-forward "^[\"\']" (line-end-position))
    (setq p2 (point))
    (buffer-substring-no-properties p1 p2)))


(defun +goto-file-at-point ()
  "Find the file at point and open it."
  (interactive)
  (let (file-path)
    (setq file-path (my-select-inside-quotes))
    (consult-fd (get-project-root) file-path)
    ))


(defun +remove-invalidate-buffers ()
  "Remove invalidate buffers"
  (interactive)
  (mapc (lambda (buf)
       (with-current-buffer buf
         (if (and (buffer-file-name buf)
                  (not (file-exists-p (buffer-file-name buf)))
                  (not (buffer-modified-p buf)))
                  (kill-buffer buf))))
        (buffer-list)))


(defun consult-line-ex ()
  (interactive)
  (if (use-region-p)
      (consult-line (buffer-substring-no-properties (region-beginning) (region-end)))
    (consult-line)))

(provide 'init-helper)
