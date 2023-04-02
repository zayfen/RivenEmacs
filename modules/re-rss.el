;;; re-rss.el --- News and RSS -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


(use-package elfeed
  :straight t
  :init
  (+map! "of" #'elfeed)
  :custom
  (elfeed-db-directory (concat rivenemacs-local-dir "elfeed/db/"))
  (elfeed-enclosure-default-dir (concat rivenemacs-local-dir "elfeed/enclosure/"))
  :config
  ;; Hide the annoying index file form recent files
  (+ignore-root elfeed-db-directory elfeed-enclosure-default-dir))


(provide 're-rss)
