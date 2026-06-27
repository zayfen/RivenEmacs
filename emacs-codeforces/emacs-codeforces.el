;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces.el --- Codeforces workflow client for Emacs -*- lexical-binding: t; -*-

;; Author: RivenEmacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, games
;; URL: https://github.com/zayfen/RivenEmacs

;;; Commentary:
;; `emacs-codeforces' is an independent Emacs Lisp library for the
;; Codeforces competitive-programming workflow.  It provides login
;; (session-cookie injection), problem browsing with tag search, Org-rendered
;; problem statements, solution scaffolding, and submission with real-time
;; verdict polling.
;;
;; Load it with (require 'emacs-codeforces).  See the `codeforces' customize
;; group for options.

;;; Code:

(require 'emacs-codeforces-client)
(require 'emacs-codeforces-auth)
(require 'emacs-codeforces-api)
(require 'emacs-codeforces-scrape)
(require 'emacs-codeforces-workspace)
(require 'emacs-codeforces-list)
(require 'emacs-codeforces-detail)

(defgroup codeforces nil
  "Codeforces workflow client."
  :group 'tools)

(defcustom codeforces-home-directory
  (expand-file-name "~/.emacs-codeforces/")
  "Codeforces workspace root.
Holds credentials, API cache, and per-problem solution directories."
  :type 'directory
  :group 'codeforces)

(defcustom codeforces-default-language "rust"
  "Default language for new solutions.
Must correspond to a template file in `codeforces-templates-directory'
or the bundled templates."
  :type '(choice (const "rust") (const "python") (const "cpp") (const "java"))
  :group 'codeforces)

(defcustom codeforces-poll-interval 2
  "Seconds between submission status polls."
  :type 'number
  :group 'codeforces)

(defcustom codeforces-poll-timeout 60
  "Max seconds to poll a submission before giving up."
  :type 'number
  :group 'codeforces)

(defcustom codeforces-templates-directory nil
  "Override directory for language templates.
Nil means use the templates bundled with the library."
  :type '(choice directory (const nil))
  :group 'codeforces)

(provide 'emacs-codeforces)

;;; emacs-codeforces.el ends here
