;; -*- lexical-binding: t -*-
;; Temporary script for startup benchmarking.
;; Load early-init then init, measure elapsed time.
;; Usage: emacs -Q --batch -l benchmark-startup.el

(let ((bench-dir (file-name-directory (file-truename load-file-name)))
      (start (float-time)))
  (setq user-emacs-directory bench-dir)
  (load (expand-file-name "early-init.el" bench-dir) nil t)
  (load (expand-file-name "init.el" bench-dir) nil t)
  (let ((elapsed (- (float-time) start)))
    (message "RivenEmacs config load: %.3f seconds" elapsed)))
