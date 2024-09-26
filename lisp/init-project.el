;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-project.el --- project managerment config

(defun get-project-buffers (&rest _)
  "Return a list of buffers that belong to the current project."
  (interactive)
  (let ((project-root (file-truename (get-project-root)))
        (project-buffers '()))
    (dolist (buf (buffer-list))
      (if (buffer-file-name buf)
          (if  (not (string-match (regexp-quote project-root) (buffer-file-name buf)))
              (push buf project-buffers))))
    project-buffers)
  )

(defun delete-other-project-buffers (&rest _)
  "Delete other project buffers"
  (interactive "p")
  (let ((other-project-buffers (get-project-buffers)))
    (dolist (buffer other-project-buffers)
      (kill-buffer buffer))
    )
  )

(defun project-switch-project-ex (dir)
  "Project switch project enhancer"
  (interactive (list (project-prompt-project-dir)))
  (progn
    (project-switch-project dir)
    (delete-other-project-buffers)
    )
  )

(provide 'init-project)
