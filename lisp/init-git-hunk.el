;;; init-git-hunk.el --- Git hunk navigation and manipulation -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 RivenEmacs
;;
;; Author: RivenEmacs
;; Keywords: git, diff, navigation
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Provides git hunk navigation and manipulation functionality.
;; Features:
;; - Navigate between git hunks with n/p
;; - Highlight git hunks like magit
;; - Reject/revert hunks with r
;; - Approve/save hunks with s
;; - Exit with C-g

;;; Code:

(require 'diff-hl nil t)

(defvar git-hunk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'git-hunk-next)
    (define-key map (kbd "p") 'git-hunk-previous)
    (define-key map (kbd "r") 'git-hunk-revert)
    (define-key map (kbd "s") 'git-hunk-stage)
    (define-key map (kbd "C-g") 'git-hunk-exit)
    map)
  "Keymap for `git-hunk-mode'.")

(define-minor-mode git-hunk-mode
  "Minor mode for navigating and manipulating git hunks."
  :lighter " GitHunk"
  :keymap git-hunk-mode-map)

(defun git-hunk-next ()
  "Jump to the next git hunk."
  (interactive)
  (diff-hl-next-hunk))

(defun git-hunk-previous ()
  "Jump to the previous git hunk."
  (interactive)
  (diff-hl-previous-hunk))

(defun git-hunk-revert ()
  "Revert the current git hunk."
  (interactive)
  (diff-hl-revert-hunk))

(defun git-hunk-stage ()
  "Stage the current git hunk."
  (interactive)
  (diff-hl-stage-current-hunk))

(global-set-key (kbd "C-c h") 'git-hunk-mode)

(provide 'init-git-hunk)
;;; init-git-hunk.el ends here