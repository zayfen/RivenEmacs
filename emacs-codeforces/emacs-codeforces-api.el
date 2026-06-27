;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-api.el --- Read-only Codeforces JSON API client -*- lexical-binding: t; -*-

;;; Commentary:
;; Wraps the public Codeforces API for problems and submission status.
;; `+cf-fetch-problems' hits `problemset.problems' and caches to
;; `cache/problems.json'.  Filtering helpers (`+cf--filter-by-tags',
;; `+cf--filter-by-rating', `+cf--all-tags') are pure and unit-tested.

;;; Code:

(require 'cl-lib)
(require 'emacs-codeforces-client)

;; Declared as `defcustom' in emacs-codeforces.el; defvar here so this module
;; can be loaded and tested standalone.
(defvar codeforces-home-directory
  (expand-file-name "~/.emacs-codeforces/")
  "Codeforces workspace root.
Also a `defcustom' in `emacs-codeforces.el'; kept here so this module
works when loaded standalone (the `defcustom' will not overwrite an
already-set value).")

(defun +cf--cache-file ()
  "Path to the problems cache."
  (expand-file-name "cache/problems.json" codeforces-home-directory))

(defun +cf--parse-problems (json-string)
  "Parse the `problemset.problems' response JSON-STRING into a list of plists.
Accepts either the full {\"status\":...,\"result\":{\"problems\":[...]}} body
or a bare {\"result\":{\"problems\":[...]}} wrapper."
  (let ((parsed (+cf--parse-json-body json-string)))
    (plist-get (or (plist-get parsed :result) parsed) :problems)))

(defun +cf--filter-by-tags (problems tags)
  "Keep PROBLEMS having ALL of TAGS (AND semantics)."
  (if (seq-empty-p tags)
      problems
    (cl-remove-if-not
     (lambda (p)
       (let ((ptags (plist-get p :tags)))
         (cl-every (lambda (tg) (member tg ptags)) tags)))
     problems)))

(defun +cf--filter-by-rating (problems lo hi)
  "Keep PROBLEMS with rating in [LO, HI] inclusive.  Nil rating excluded."
  (cl-remove-if-not
   (lambda (p)
     (let ((r (plist-get p :rating)))
       (and r (>= r lo) (<= r hi))))
   problems))

(defun +cf--all-tags (problems)
  "Return the sorted, deduplicated union of all tags in PROBLEMS."
  (sort (cl-remove-duplicates
         (cl-reduce #'append (mapcar (lambda (p) (plist-get p :tags)) problems))
         :test #'string=)
        #'string<))

(defun +cf-fetch-problems (&optional force)
  "Fetch the problem list, caching the raw response locally.  FORCE re-fetches.
Returns a list of problem plists."
  (let ((cache (+cf--cache-file)))
    (when (or force (not (file-exists-p cache)))
      (let* ((resp (+cf-http-get (+cf--merge-query
                                  (concat +cf-api-base "problemset.problems")
                                  nil))))
        (unless (= (car resp) 200)
          (error "Codeforces API problemset.problems failed (HTTP %s)" (car resp)))
        (make-directory (file-name-directory cache) t)
        (let ((coding-system-for-write 'utf-8))
          (with-temp-file cache
            (insert (cdr resp))))))
    (+cf--parse-problems
     (with-temp-buffer
       (insert-file-contents cache)
       (buffer-string)))))

(defun +cf-fetch-recent-submissions (handle count)
  "Fetch the COUNT most recent submissions for HANDLE via `user.status'.
Returns a list of submission plists, newest first.  Each has :id :verdict
(may be nil while queued) :passedTestCount :timeConsumedMillis
:memoryConsumedBytes :contestId :creationTimeSeconds.  No auth needed."
  (+cf-api-get "user.status"
               :handle handle
               :from 1
               :count count))

(defun +cf-fetch-submission (contest-id submission-id)
  "Fetch a single submission's status from `contest.status'.
CONTEST-ID is the contest number; SUBMISSION-ID is a string or number.
Returns the matching submission plist (with :verdict :passedTestCount
:timeConsumedMillis :memoryConsumedBytes), or nil if not found.  No auth
needed — used by the verdict poller after the agent returns the id."
  (let* ((result (+cf-api-get "contest.status"
                              :contestId contest-id
                              :from 1
                              :count 100)))
    (cl-find (if (stringp submission-id)
                 (string-to-number submission-id)
               submission-id)
             result
             :key (lambda (s) (plist-get s :id))
             :test #'equal)))

(defun +cf-find-problem-submission (handle contest-id since-time)
  "Find HANDLE's most recent submission for CONTEST-ID at/after SINCE-TIME.
Returns the submission plist, or nil if none yet.  Used by the verdict poller:
SINCE-TIME is the epoch seconds when the user pressed submit, so the first
matching submission is the one being tracked."
  (cl-find-if
   (lambda (sub)
     (and (eq (plist-get sub :contestId) contest-id)
          (>= (or (plist-get sub :creationTimeSeconds) 0) since-time)))
   (+cf-fetch-recent-submissions handle 5)))

(provide 'emacs-codeforces-api)

;;; emacs-codeforces-api.el ends here
