;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-list.el --- Problem list buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; `codeforces-browse-problems' opens `*Codeforces Problems*' as a
;; `tabulated-list-mode' derivative.  Columns: ID, Name, Rating, Tags.
;; `/` filters by tags (consult multi-select, AND), `r` by rating range,
;; `RET` opens the statement in a right-side window, `g` refreshes.

;;; Code:

(require 'tabulated-list)
(require 'emacs-codeforces-api)
(require 'emacs-codeforces-detail)

(defvar-local +cf--all-problems nil
  "All problems fetched (unfiltered).")

(defvar-local +cf--tag-filter nil
  "Current tag filter (list of strings), nil = no filter.")

(defvar-local +cf--rating-filter nil
  "Current rating filter (lo . hi), nil = no filter.")

(defun +cf--problem-id (p)
  "Compact id for PROBLEM, e.g. \"1234A\"."
  (format "%s%s" (plist-get p :contestId) (plist-get p :index)))

(defun +cf--format-tags (tags)
  "Join TAGS with \", \"."
  (mapconcat #'identity (or tags '("")) ", "))

(defun +cf--problem->-entry (p)
  "Convert PROBLEM plist to a tabulated-list entry [id name rating tags] . P."
  (list p
        (vector (+cf--problem-id p)
                (or (plist-get p :name) "")
                (number-to-string (or (plist-get p :rating) 0))
                (+cf--format-tags (plist-get p :tags)))))

(defun +cf--apply-filters ()
  "Recompute the tabulated entries from current filters."
  (let ((problems +cf--all-problems))
    (when +cf--tag-filter
      (setq problems (+cf--filter-by-tags problems +cf--tag-filter)))
    (when +cf--rating-filter
      (setq problems (+cf--filter-by-rating problems
                                            (car +cf--rating-filter)
                                            (cdr +cf--rating-filter))))
    (setq tabulated-list-entries
          (mapcar #'+cf--problem->-entry problems))
    (tabulated-list-print t)))

(defun +cf--open-at-point ()
  "Open the statement for the problem at point."
  (interactive)
  (let ((p (tabulated-list-get-id)))
    (when p
      (codeforces-open-problem p))))

(defun +cf--read-tags (problems)
  "Interactively read multiple tags via `consult', candidates from PROBLEMS."
  (if (require 'consult nil 'noerror)
      (consult-completing-read-multiple
       (+cf--all-tags problems)
       "Tags (AND): " nil t)
    (completing-read-multiple "Tags (AND): " (+cf--all-tags problems))))

(defun codeforces-filter-by-tags (tags)
  "Set the tag filter to TAGS (list of strings).  Empty clears."
  (interactive (list (+cf--read-tags +cf--all-problems)))
  (setq +cf--tag-filter (if (seq-empty-p tags) nil tags))
  (+cf--apply-filters))

(defun codeforces-filter-by-rating (range)
  "Set the rating filter to RANGE (\"lo-hi\").  Empty clears."
  (interactive
   (list (read-string "Rating range (e.g. 800-1500, empty to clear): "
                      (if +cf--rating-filter
                          (format "%s-%s" (car +cf--rating-filter) (cdr +cf--rating-filter))))))
  (setq +cf--rating-filter
        (if (or (null range) (string-empty-p range))
            nil
          (if (string-match "\\([0-9]+\\)-\\([0-9]+\\)" range)
              (cons (string-to-number (match-string 1 range))
                    (string-to-number (match-string 2 range)))
            (error "Bad range: %s" range))))
  (+cf--apply-filters))

(defun +cf--refresh ()
  "Refresh the problem list from the API."
  (interactive)
  (setq +cf--all-problems (+cf-fetch-problems 'force))
  (+cf--apply-filters))

(define-derived-mode codeforces-problems-mode tabulated-list-mode "CF Problems"
  "Major mode for browsing Codeforces problems.
\\<codeforces-problems-mode>
\\{codeforces-problems-mode}"
  (setq tabulated-list-format [("ID" 8 t)
                               ("Name" 50 t)
                               ("Rating" 8 t)
                               ("Tags" 40 nil)]
        tabulated-list-sort-key (cons "ID" nil)
        tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun +cf--buffer ()
  "Return (creating if needed) the problems buffer."
  (or (get-buffer "*Codeforces Problems*")
      (with-current-buffer (get-buffer-create "*Codeforces Problems*")
        (codeforces-problems-mode)
        (current-buffer))))

;;;###autoload
(defun codeforces-browse-problems ()
  "Open the Codeforces problem list."
  (interactive)
  (let ((buf (+cf--buffer)))
    (with-current-buffer buf
      (setq +cf--all-problems (+cf-fetch-problems))
      (+cf--apply-filters))
    (pop-to-buffer buf)))

(let ((map codeforces-problems-mode-map))
  (define-key map (kbd "RET") #'+cf--open-at-point)
  (define-key map (kbd "/") #'codeforces-filter-by-tags)
  (define-key map (kbd "r") #'codeforces-filter-by-rating)
  (define-key map (kbd "g") #'+cf--refresh))

(provide 'emacs-codeforces-list)

;;; emacs-codeforces-list.el ends here
