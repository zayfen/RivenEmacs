;;; re-binary.el --- Stuff to work with binary files -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


;; +binary-* are autoloaded
(+deferred!
 ;; BUG: Loading continuously on `dirvish'.
 ;; BUG: Showing up randomly on `tramp' files.
 (setq +binary-objdump-enable nil)
 (+binary-setup-modes))

(provide 're-binary)
