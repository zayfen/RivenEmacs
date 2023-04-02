;;; re-org-export-async-init.el --- An init file for exporting Org documents asynchronously -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;; This file will be used as `user-init-file' when exporting Org documents
;; asynchronously. This will set the modules list to the minimal required to
;; export Org documents.

;; BUG: For some reason, exporting in background can fail with this error:
;; (error "Odd length text property list"), in such case, you can remove the Org
;; cache directory and retry again: rm -rf ~/.emacs.d/local/cache/org/ (see
;; github.com/org-roam/org-roam/issues/2155#issuecomment-1145388814)

(message "Using RivenEmacs' \"re-org-export-async-init.el\" as init file.")

;; This signals that we are running in a org-export-async context
(provide 're-org-export-async-init)

;; Load only some essential modules
(setq rivenemacs-core-modules nil
      rivenemacs-modules
      '(re-org re-biblio re-latex re-project re-prog re-data re-lisp))

(load (concat user-emacs-directory "init.el") nil t)

(message "Loaded %d modules!" (+ (length rivenemacs-core-modules) (length rivenemacs-modules)))
