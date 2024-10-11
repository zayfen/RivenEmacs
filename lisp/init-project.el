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

;; (use-package otpp
;;   :vc (:fetcher github :repo "abougouffa/one-tab-per-project")
;;   :after project
;;   :init
;;   ;; If you like to define some aliases for better user experience
;;   (defalias 'one-tab-per-project-mode 'otpp-mode)
;;   (defalias 'one-tab-per-project-override-mode 'otpp-override-mode)
;;   ;; Enable `otpp-mode` globally
;;   (otpp-mode 1)
;;   ;; If you want to advice the commands in `otpp-override-commands`
;;   ;; to be run in the current's tab (so, current project's) root directory
;;   (otpp-override-mode 1))

(provide 'init-project)
