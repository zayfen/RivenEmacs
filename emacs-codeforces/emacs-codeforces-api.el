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
  "Parse the `problemset.problems' result JSON-STRING into a list of plists.
The cache JSON wraps the API result as {\"result\":{\"problems\":[...]}}."
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
  "Fetch the problem list, caching locally.  FORCE re-fetches.
Returns a list of problem plists."
  (let ((cache (+cf--cache-file)))
    (when (or force (not (file-exists-p cache)))
      (let* ((raw (+cf-api-get "problemset.problems")))
        (make-directory (file-name-directory cache) t)
        (with-temp-file cache
          (insert (json-encode `(:result (:problems ,raw)))))))
    (+cf--parse-problems
     (with-temp-buffer
       (insert-file-contents cache)
       (buffer-string)))))

(defun +cf-fetch-submission (contest-id submission-id)
  "Fetch a single submission's status from `contest.status'.
Returns a plist with at least :id :verdict :passedTestCount :timeConsumedMillis
:memoryConsumedBytes.  :verdict may be nil while queued."
  (let* ((result (+cf-api-get "contest.status"
                              :contestId contest-id
                              :from 1
                              :count 100))
         (subs result)
         (found (cl-find submission-id subs
                        :key (lambda (s) (plist-get s :id))
                        :test #'equal)))
    (or found
        (error "Submission %s not found in contest %s" submission-id contest-id))))

(provide 'emacs-codeforces-api)

;;; emacs-codeforces-api.el ends here
