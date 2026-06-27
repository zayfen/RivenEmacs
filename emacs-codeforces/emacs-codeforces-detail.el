;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-detail.el --- Problem detail Org buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; `codeforces-open-problem' opens `*CF <id>*', an Org buffer rendering the
;; statement.  `C-c C-c' starts solving (creates workspace dir + solution file),
;; `C-c C-s' submits and polls verdicts in-place.  The detail buffer uses
;; `olivetti-mode'/`org-modern-mode' if available (reusing RivenEmacs org setup).

;;; Code:

(require 'org)
(require 'emacs-codeforces-api)
(require 'emacs-codeforces-scrape)
(require 'emacs-codeforces-workspace)

;; Declared as `defcustom' in emacs-codeforces.el; defvar here so this module
;; can be loaded and tested standalone.
(defvar codeforces-default-language "rust")
(defvar codeforces-poll-interval 2)
(defvar codeforces-poll-timeout 60)

(defvar-local +cf--current-problem nil
  "The problem plist for this detail buffer.")

(defvar-local +cf--solution-language nil
  "Language chosen for this problem's solution.")

(defvar-local +cf--poll-timer nil
  "Active submission poll timer, if any.")

(defvar-local +cf--poll-start nil
  "Timestamp the current poll started.")

(defun +cf--problem-id (problem)
  "Compact id, e.g. \"1234A\"."
  (format "%s%s" (plist-get problem :contestId) (plist-get problem :index)))

(defun +cf--format-tags (tags)
  "Join TAGS with \", \"."
  (mapconcat #'identity (or tags '("")) ", "))

(defun +cf--buffer-name (problem)
  "Buffer name for PROBLEM."
  (format "*CF %s*" (+cf--problem-id problem)))

(defun +cf--render-header (problem)
  "Return the Org header/property block for PROBLEM."
  (format "* %s - %s%s                              :codeforces:
:PROPERTIES:
:Rating: %s
:Time-Limit: %s s
:Memory-Limit: %s MB
:URL: %sproblemset/problem/%s/%s
:END:
"
          (+cf--problem-id problem)
          (or (plist-get problem :name) "")
          (if +cf--solution-language " (solving)" "")
          (or (plist-get problem :rating) "-")
          (or (plist-get problem :timeLimit) "-")
          (or (plist-get problem :memoryLimit) "-")
          +cf-site-base
          (plist-get problem :contestId)
          (plist-get problem :index)))

(defun +cf--render-tags (problem)
  "Return the tags section for PROBLEM."
  (format "** Tags\n%s\n\n"
          (+cf--format-tags (plist-get problem :tags))))

(defun +cf--render-status (text)
  "Return the submission status section with TEXT."
  (format "** Submission Status\n%s\n" (or text "*No active submission.*")))

(defun +cf--format-verdict (sub)
  "Format a submission plist SUB as a human-readable status string."
  (let ((v (plist-get sub :verdict))
        (passed (plist-get sub :passedTestCount))
        (time (plist-get sub :timeConsumedMillis))
        (mem (plist-get sub :memoryConsumedBytes)))
    (cond
     ((null v) "Queued / compiling…")
     ((eq v 'TESTING)
      (format "Running… passed %s tests." (or passed 0)))
     ((eq v 'OK)
      (format "OK ✅  (%sms, %sMB)"
              (or time 0)
              (/ (or mem 0) 1048576)))
     (t (format "%s ❌  (passed %s tests)"
                v (or passed 0))))))

(defun +cf--write-buffer (problem body)
  "Render PROBLEM with BODY (the Org statement) into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (+cf--render-header problem))
    (insert (+cf--render-tags problem))
    (insert "** Statement\n")
    (insert (or body "(could not fetch statement)"))
    (insert "\n\n")
    (insert (+cf--render-status nil))))

(defun +cf--update-status (text)
  "Replace the Submission Status section in the current buffer with TEXT."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\* Submission Status\n" nil t)
        (delete-region (point) (point-max))
        (insert (or text ""))))))

(defun +cf--stop-poll ()
  "Stop the current poll timer."
  (when (timerp +cf--poll-timer)
    (cancel-timer +cf--poll-timer))
  (setq +cf--poll-timer nil))

(defun +cf--terminal-verdict-p (verdict)
  "Return non-nil if VERDICT is terminal."
  (memq verdict '(OK WRONG_ANSWER TIME_LIMIT_EXCEEDED
                  MEMORY_LIMIT_EXCEEDED RUNTIME_ERROR COMPILATION_ERROR
                  PARTIAL IDLENESS_LIMIT_EXCEEDED CHALLENGED
                  SKIPPED REJECTED FAILED)))

(defun +cf--poll-once (contest-id submission-id)
  "Poll once and update the status section; stop when terminal or timed out."
  (condition-case err
      (let* ((sub (+cf-fetch-submission contest-id submission-id))
             (text (+cf--format-verdict sub))
             (v (plist-get sub :verdict)))
        (+cf--update-status text)
        (when (or (+cf--terminal-verdict-p v)
                  (> (- (float-time) +cf--poll-start) codeforces-poll-timeout))
          (+cf--stop-poll)
          (when (+cf--terminal-verdict-p v)
            (message "Codeforces: %s" text))))
    (error
     (message "Codeforces poll error: %s" (error-message-string err))
     (+cf--stop-poll))))

(defun +cf--start-poll (contest-id submission-id)
  "Start polling submission status every `codeforces-poll-interval' seconds."
  (+cf--stop-poll)
  (setq +cf--poll-start (float-time))
  (setq +cf--poll-timer
        (run-with-timer
         codeforces-poll-interval codeforces-poll-interval
         #'+cf--poll-once contest-id submission-id)))

(defun +cf--solution-file (problem)
  "Return the path to PROBLEM's solution file, or nil."
  (when +cf--solution-language
    (expand-file-name (concat "solution."
                              (+cf--language-extension +cf--solution-language))
                      (+cf--problem-dir problem))))

(defun +cf--choose-language ()
  "Prompt for a solution language, defaulting to `codeforces-default-language'."
  (completing-read "Language: "
                   '("rust" "python" "cpp" "java")
                   nil t nil nil
                   codeforces-default-language))

(defun codeforces-start-solving ()
  "Start solving the current problem: pick language, init workspace, open file."
  (interactive)
  (unless +cf--current-problem
    (error "No problem in this buffer"))
  (unless +cf--solution-language
    (setq +cf--solution-language (+cf--choose-language)))
  (let* ((problem +cf--current-problem)
         (sol (+cf-init-solution problem +cf--solution-language)))
    ;; Re-render to reflect the "solving" marker.
    (condition-case nil
        (+cf--write-buffer problem (+cf-fetch-problem-org problem))
      (error
       (message "Could not refresh statement (network); keeping buffer.")))
    (find-file-other-window sol)
    (message "Solving %s in %s" (+cf--problem-id problem) +cf--solution-language)))

(defun +cf--read-source (path)
  "Return file contents at PATH, or error."
  (if (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))
    (error "Solution file not found: %s" path)))

(defun codeforces-submit ()
  "Submit the current problem's solution and start polling.
Falls back to the browser on Cloudflare."
  (interactive)
  (unless +cf--current-problem
    (error "No problem in this buffer"))
  (let* ((problem +cf--current-problem)
         (sol (+cf--solution-file problem)))
    (unless sol
      (error "Run C-c C-c to start solving first"))
    (unless +cf--solution-language
      (error "Language not set"))
    (let ((source (+cf--read-source sol)))
      (+cf--update-status "Submitting…")
      (condition-case err
          (let ((result (+cf-submit-via-http problem source +cf--solution-language)))
            (cond
             ((eq result 'cloudflare)
              (+cf-submit-via-browser problem +cf--solution-language)
              (+cf--update-status
               "Submit blocked by Cloudflare — complete in browser. Polling skipped (no id)."))
             (t
              (+cf--update-status
               (format "Submitted as %s. Polling…" result))
              (+cf--start-poll (plist-get problem :contestId) result))))
        (error
         (+cf--update-status (format "Submit failed: %s"
                                     (error-message-string err))))))))

;;;###autoload
(defun codeforces-open-problem (problem)
  "Open PROBLEM (plist) in an Org detail buffer on the right."
  (interactive)
  (let ((buf (get-buffer-create (+cf--buffer-name problem))))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode)
        (setq buffer-read-only t)
        (use-local-map (copy-keymap org-mode-map))
        (local-set-key (kbd "C-c C-c") #'codeforces-start-solving)
        (local-set-key (kbd "C-c C-s") #'codeforces-submit))
      (setq +cf--current-problem problem)
      (let ((org (condition-case nil
                     (+cf-fetch-problem-org problem)
                   (error "(could not fetch statement — check login/network)"))))
        (+cf--write-buffer problem org)))
    (display-buffer buf
                    '((display-buffer-in-direction)
                      (direction . right)
                      (window-width . 0.5)))
    buf))

(provide 'emacs-codeforces-detail)

;;; emacs-codeforces-detail.el ends here
