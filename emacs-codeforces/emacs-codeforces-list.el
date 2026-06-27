;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-list.el --- Problem list buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; `codeforces-browse-problems' opens `*Codeforces Problems*' as a
;; `tabulated-list-mode' derivative.  Columns: ID, Name, Rating, Tags.
;;
;; Two data sources, switched with `TAB':
;;   - problemset (default): all public problems via `problemset.problems'
;;   - contest:               problems of a single contest, picked in the
;;                            minibuffer from `contest.list' (filtered from
;;                            the cached problemset)
;;
;; `/` filters by tags (consult multi-select, AND), `r` by rating range,
;; `RET` opens the statement in a right-side window, `g` refreshes.

;;; Code:

(require 'tabulated-list)
(require 'emacs-codeforces-api)
(require 'emacs-codeforces-detail)

(defvar-local +cf--all-problems nil
  "All problems in the current view (unfiltered).")

(defvar-local +cf--view 'problemset
  "Current list view: `problemset' or `contest'.")

(defvar-local +cf--contest-id nil
  "When `+cf--view' is `contest', the contest id being shown.")

(defvar-local +cf--contest-name nil
  "When `+cf--view' is `contest', the contest name (for the header).")

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
  "Refresh the problem list for the current view."
  (interactive)
  (cond
   ((eq +cf--view 'problemset)
    (setq +cf--all-problems (+cf-fetch-problems 'force)))
   (+cf--contest-id
    ;; Contest problems come from the problemset cache; force-refresh that.
    (+cf-fetch-problems 'force)
    (setq +cf--all-problems (+cf-fetch-contest-problems +cf--contest-id))))
  (+cf--apply-filters))

(defun +cf--pick-contest ()
  "Prompt for a contest via `contest.list' (id or name).  Return (id . name)."
  (let* ((contests (+cf-fetch-contests))
         (alist (mapcar (lambda (c)
                          (cons (format "%s  %s" (plist-get c :id)
                                        (plist-get c :name))
                                (cons (plist-get c :id) (plist-get c :name))))
                        contests))
         (choice (completing-read "Contest: " alist nil t))
         (pick (cdr (assoc choice alist))))
    (or pick (error "No contest selected"))))

(defun +cf--switch-view ()
  "Switch between problemset and contest views (bound to TAB)."
  (interactive)
  (if (eq +cf--view 'problemset)
      ;; -> contest: pick a contest, load its problems.
      (let ((pick (+cf--pick-contest)))
        (setq +cf--view 'contest
              +cf--contest-id (car pick)
              +cf--contest-name (cdr pick)
              +cf--tag-filter nil
              +cf--rating-filter nil)
        (message "Loading contest %s..." +cf--contest-id)
        (setq +cf--all-problems (+cf-fetch-contest-problems +cf--contest-id))
        (+cf--update-header)
        (+cf--apply-filters))
    ;; -> problemset (default): show the full problemset.
    (setq +cf--view 'problemset
          +cf--contest-id nil
          +cf--contest-name nil
          +cf--tag-filter nil
          +cf--rating-filter nil)
    (+cf--update-header)
    (+cf--load-view)))

(defun +cf--update-header ()
  "Update the buffer name + header line to reflect the current view."
  (cond
   ((eq +cf--view 'contest)
    (rename-buffer (format "*Codeforces: Contest %s*" +cf--contest-id) t))
   (t
    (rename-buffer "*Codeforces Problems*" t))))

(defun +cf--load-view ()
  "Load the data for the current view and refresh the entries."
  (cond
   ((eq +cf--view 'contest)
    (setq +cf--all-problems (+cf-fetch-contest-problems +cf--contest-id)))
   (t
    (setq +cf--all-problems (+cf-fetch-problems))))
  (+cf--apply-filters))

(define-derived-mode codeforces-problems-mode tabulated-list-mode "CF Problems"
  "Major mode for browsing Codeforces problems.
\\<codeforces-problems-mode>
TAB switches between the full problemset and a single contest's problems.
\\{codeforces-problems-mode}"
  (setq tabulated-list-format [("ID" 8 t)
                               ("Name" 50 t)
                               ("Rating" 8 t)
                               ("Tags" 40 nil)]
        tabulated-list-sort-key (cons "ID" nil)
        tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun +cf--buffer ()
  "Return (creating if needed) the problems buffer, in problemset view."
  (let ((buf (get-buffer-create "*Codeforces Problems*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'codeforces-problems-mode)
        (codeforces-problems-mode)))
    buf))

;;;###autoload
(defun codeforces-browse-problems ()
  "Open the Codeforces problem list (problemset view by default).
Press TAB to switch to a single contest's problems."
  (interactive)
  (let ((buf (+cf--buffer)))
    (with-current-buffer buf
      ;; Default to problemset on each entry.
      (setq +cf--view 'problemset
            +cf--contest-id nil
            +cf--contest-name nil
            +cf--tag-filter nil
            +cf--rating-filter nil)
      (+cf--update-header)
      (+cf--load-view))
    (pop-to-buffer buf)))

(let ((map codeforces-problems-mode-map))
  (define-key map (kbd "TAB") #'+cf--switch-view)
  (define-key map (kbd "RET") #'+cf--open-at-point)
  (define-key map (kbd "/") #'codeforces-filter-by-tags)
  (define-key map (kbd "r") #'codeforces-filter-by-rating)
  (define-key map (kbd "g") #'+cf--refresh))

(provide 'emacs-codeforces-list)

;;; emacs-codeforces-list.el ends here
