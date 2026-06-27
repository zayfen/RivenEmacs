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
(require 'emacs-codeforces-auth)

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
     ((equal v "TESTING")
      (format "Running… passed %s tests." (or passed 0)))
     ((equal v "OK")
      (format "OK ✅  (%sms, %sMB)"
              (or time 0)
              (/ (or mem 0) 1048576)))
     (t (format "%s ❌  (passed %s tests)"
                v (or passed 0))))))

(defun +cf--write-buffer (problem)
  "Render PROBLEM's metadata into the current Org buffer.
The full statement is Cloudflare-blocked for direct fetch, so this buffer
shows the API metadata (name, tags, limits, URL) plus a pointer to open the
statement in the browser (key `o')."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (+cf--render-header problem))
    (insert (+cf--render-tags problem))
    (insert "** Statement\n")
    (insert "The full statement renders in the browser (Cloudflare blocks direct fetch).\n")
    (insert "Press =o= to open it: ")
    (insert (+cf--problem-url problem))
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
  (member (and verdict (if (symbolp verdict) (symbol-name verdict) verdict))
          '("OK" "WRONG_ANSWER" "TIME_LIMIT_EXCEEDED"
            "MEMORY_LIMIT_EXCEEDED" "RUNTIME_ERROR" "COMPILATION_ERROR"
            "PARTIAL" "IDLENESS_LIMIT_EXCEEDED" "CHALLENGED"
            "SKIPPED" "REJECTED" "FAILED")))

(defun +cf--poll-once (handle contest-id since-epoch)
  "Poll once: find HANDLE's submission for CONTEST-ID at/after SINCE-EPOCH.
Update the status section; stop when a terminal verdict or poll-timeout."
  (condition-case err
      (let ((sub (+cf-find-problem-submission handle contest-id since-epoch)))
        (cond
         (sub
          (let ((text (+cf--format-verdict sub))
                (v (plist-get sub :verdict)))
            (+cf--update-status text)
            (when (or (+cf--terminal-verdict-p v)
                      (> (- (float-time) +cf--poll-start) codeforces-poll-timeout))
              (+cf--stop-poll)
              (when (+cf--terminal-verdict-p v)
                (message "Codeforces: %s" text)))))
         (t
          ;; No matching submission yet (user may still be submitting in browser).
          (+cf--update-status "Waiting for submission to appear…")))
        (when (> (- (float-time) +cf--poll-start) codeforces-poll-timeout)
          (+cf--stop-poll)
          (+cf--update-status "Timed out waiting for the submission. Press C-c C-s to retry.")))
    (error
     (message "Codeforces poll error: %s" (error-message-string err))
     (+cf--stop-poll))))

(defun +cf--start-poll (handle contest-id since-epoch)
  "Start polling HANDLE's submission for CONTEST-ID at/after SINCE-EPOCH.
Polls every `codeforces-poll-interval' seconds until a terminal verdict or
`codeforces-poll-timeout' seconds elapse."
  (+cf--stop-poll)
  (setq +cf--poll-start (float-time))
  (setq +cf--poll-timer
        (run-with-timer
         codeforces-poll-interval codeforces-poll-interval
         #'+cf--poll-once handle contest-id since-epoch)))

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
    (+cf--write-buffer problem)
    (find-file-other-window sol)
    (message "Solving %s in %s" (+cf--problem-id problem) +cf--solution-language)))

(defun +cf--read-source (path)
  "Return file contents at PATH, or error."
  (if (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))
    (error "Solution file not found: %s" path)))

(defun codeforces-open-statement ()
  "Open the current problem's full statement in the browser."
  (interactive)
  (unless +cf--current-problem
    (error "No problem in this buffer"))
  (+cf-open-statement-in-browser +cf--current-problem))

(defun codeforces-submit ()
  "Open the submit page in the browser and start polling the verdict.
The submit itself happens in the browser (Cloudflare blocks direct Emacs
POST); Emacs polls the public API for the resulting verdict."
  (interactive)
  (unless +cf--current-problem
    (error "No problem in this buffer"))
  (let* ((problem +cf--current-problem)
         (handle (+cf--handle)))
    (unless handle
      (error "Not logged in.  Run M-x codeforces-login to store your handle"))
    (let ((sol (+cf--solution-file problem)))
      (unless sol
        (error "Run C-c C-c to start solving first")))
    ;; Open the browser submit page (user pastes + submits there).
    (+cf-submit-via-browser problem)
    ;; Poll the public API for the verdict, starting now so we catch the
    ;; new submission even if the user is slow in the browser.
    (+cf--update-status "Submit page opened in browser. Polling for verdict…")
    (let ((since-epoch (truncate (float-time))))
      (+cf--start-poll handle (plist-get problem :contestId) since-epoch))))

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
        (local-set-key (kbd "C-c C-s") #'codeforces-submit)
        (local-set-key (kbd "o") #'codeforces-open-statement))
      (setq +cf--current-problem problem)
      (+cf--write-buffer problem))
    (display-buffer buf
                    '((display-buffer-in-direction)
                      (direction . right)
                      (window-width . 0.5)))
    buf))

(provide 'emacs-codeforces-detail)

;;; emacs-codeforces-detail.el ends here
