;; -*- coding: utf-8; lexical-binding: t; -*-

;;; init-codeforces.el --- RivenEmacs integration for emacs-codeforces -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin wrapper that loads the `emacs-codeforces' library from the repo-local
;; `emacs-codeforces/' directory and registers its keybinding spec.
;;
;; Leader keys (under `C-c f'):
;;   C-c f l  codeforces-login
;;   C-c f L  codeforces-logout
;;   C-c f p  codeforces-browse-problems

;;; Code:

(use-package emacs-codeforces
  :load-path (lambda () (list (expand-file-name "emacs-codeforces" root-dir)))
  :commands (codeforces-login codeforces-logout codeforces-browse-problems)
  :custom
  (codeforces-default-language "rust"))

(provide 'init-codeforces)
;;; init-codeforces.el ends here
