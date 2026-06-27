;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-scrape.el --- Browser delegation for Codeforces -*- lexical-binding: t; -*-

;;; Commentary:
;; The Codeforces website (statement pages, submit form) is behind Cloudflare
;; and unreachable from Emacs, so this module delegates those operations to the
;; user's browser via `browse-url'.  It keeps:
;;   - URL construction for problem statements and the submit page.
;;   - A Cloudflare-detection helper (for diagnostics / future use).
;;
;; All data Emacs renders itself comes from the public JSON API
;; (see `emacs-codeforces-api.el').

;;; Code:

(require 'emacs-codeforces-client)

(defun +cf--problem-url (problem)
  "Return the website URL for PROBLEM's statement."
  (format "%sproblemset/problem/%s/%s"
          +cf-site-base
          (plist-get problem :contestId)
          (plist-get problem :index)))

(defun +cf--submit-url (problem)
  "Return the contest submit URL for PROBLEM."
  (format "%scontest/%s/submit"
          +cf-site-base
          (plist-get problem :contestId)))

(defun +cf--detect-cloudflare (body)
  "Return non-nil if BODY looks like a Cloudflare challenge page.
Kept for diagnostics; submit/statement now bypass the website entirely."
  (or (string-match-p "Just a moment" (or body ""))
      (string-match-p "cf-chl" (or body ""))
      (string-match-p "cf-browser-verification" (or body ""))))

(defun +cf-open-statement-in-browser (problem)
  "Open PROBLEM's statement in the browser (logged in, past Cloudflare)."
  (browse-url (+cf--problem-url problem))
  (message "Statement opened in browser: %s" (+cf--problem-url problem)))

(defun +cf-submit-via-browser (problem)
  "Open PROBLEM's submit page in the browser.
The user is logged in there and past Cloudflare, so they paste their solution
and submit.  Emacs polls the verdict via the public API meanwhile."
  (browse-url (+cf--submit-url problem))
  (message "Submit page opened in browser.  Paste your solution and submit; the verdict is polled here."))

(provide 'emacs-codeforces-scrape)

;;; emacs-codeforces-scrape.el ends here
