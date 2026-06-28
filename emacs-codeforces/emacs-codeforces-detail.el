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
(require 'emacs-codeforces-agent)

(defvar codeforces-problem-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "a") #'codeforces-start-solving)
    (define-key map (kbd "s") #'codeforces-submit)
    (define-key map (kbd "g") #'codeforces-refresh-statement)
    (define-key map (kbd "o") #'codeforces-open-statement)
    (define-key map (kbd "p") #'codeforces-poll-latest)
    (define-key map (kbd "?") #'codeforces-problem-help)
    map)
  "Keymap for `codeforces-problem-mode'.")

(defconst codeforces-problem-mode-bindings
  '((?q . "Close the buffer")
    (?a . "Accept the problem (start solving)")
    (?s . "Submit the solution and poll the verdict")
    (?g . "Re-fetch the statement from the site")
    (?o . "Open the problem page in the browser")
    (?p . "Poll the latest submission status"))
  "Alist of (key-char . description) shown by `codeforces-problem-help'.")

(defun codeforces-problem-help ()
  "Show the key bindings for `codeforces-problem-mode' in the echo area."
  (interactive)
  (message
    "%s"
    (mapconcat
      (lambda (cell)
        (format "%c  %s" (car cell) (cdr cell)))
      codeforces-problem-mode-bindings "    ")))

(defun codeforces-diagnose-math ()
  "Diagnostic: report math-fragment recognition and overlay coverage.
Run this in a problem detail buffer to see why $...$ delimiters may show."
  (interactive)
  (let ((frags 0)
        (covered-fragments 0)
        (sample-bad nil)
        (backend (bound-and-true-p org-preview-latex-default-process))
        (graphic (display-graphic-p))
        (untrusted (bound-and-true-p untrusted-content))
        (overlays-with-image 0)
        (total-dollar-positions 0)
        (covered-dollar-positions 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\$" nil t)
        (cl-incf total-dollar-positions)
        (let ((o (car (overlays-at (point)))))
          (when (and o (overlay-get o 'display))
            (cl-incf covered-dollar-positions))))
      (setq overlays-with-image
            (length (delq nil (mapcar (lambda (o)
                                        (let ((d (overlay-get o 'display)))
                                          (and (consp d) (eq (car d) 'image))))
                                      (overlays-in (point-min) (point-max)))))))
    (let ((parsed (org-element-parse-buffer)))
      (org-element-map parsed 'latex-fragment
        (lambda (f) (cl-incf frags))))
    (message
     (concat "MATH-DIAGNOSE:\n"
             "  display-graphic-p: %s\n"
             "  preview-backend: %s\n"
             "  untrusted-content: %s\n"
             "  latex-fragments-recognized: %d\n"
             "  image-overlays: %d\n"
             "  total-$-chars: %d\n"
             "  $-chars-covered-by-overlay: %d\n")
     graphic backend untrusted frags overlays-with-image
     total-dollar-positions covered-dollar-positions)
    ;; Direct render test: create one image and report the result/ error.
    (let ((value "$y=x^2$")
          (dir (make-temp-file "cf-latex-" t))
          (render-err nil)
          (rendered nil))
      (condition-case err
          (let ((movefile (expand-file-name "cf-test.png" dir)))
            (org-create-formula-image
             value movefile org-format-latex-options 'forbuffer
             org-preview-latex-default-process)
            (setq rendered (file-exists-p movefile)))
        (error (setq render-err (error-message-string err))))
      (message
       "  direct-render: value=%s backend=%s rendered=%s err=%s"
       value org-preview-latex-default-process rendered
       (or render-err "none"))
      ;; Check *Org Preview LaTeX Output* for the actual shell error.
      (when (get-buffer "*Org Preview LaTeX Output*")
        (with-current-buffer "*Org Preview LaTeX Output*"
          (message "  org-preview-log: %s"
                   (string-trim (buffer-string))))))))

;;;###autoload
(define-derived-mode codeforces-problem-mode org-mode "CF Problem"
  "Major mode for viewing a Codeforces problem statement.
Inherits Org mode (so the statement renders fully) and adds Codeforces
workflow keys:
  \\{codeforces-problem-mode-map}
- `q' close the buffer
- `a' accept the problem (start solving, scaffold the solution file)
- `s' submit the current solution and poll the verdict
- `g' re-fetch the statement from the site
- `o' open the problem page in the browser
- `p' poll the latest submission status for this problem
- `?' show these key bindings")
(set-keymap-parent codeforces-problem-mode-map org-mode-map)

;; Ensure Org treats $...$ and $$...$$ as LaTeX fragments so they are fontified
;; (and previewable) rather than shown as literal $ delimiters.
(with-eval-after-load 'org
  (unless (memq 'latex (bound-and-true-p org-highlight-latex-and-related))
    (add-to-list 'org-highlight-latex-and-related 'latex)))

;; Register a math-preview backend that compiles with xelatex (so it works with
;; RivenEmacs's global fontspec) and converts to PNG via imagemagick.  The
;; built-in `xelatex' backend requires dvisvgm, which is often missing, whereas
;; `convert' is usually installed.  This is added once globally (idempotent).
(with-eval-after-load 'org
  (unless (assq 'xelatex-imagemagick org-preview-latex-process-alist)
    (add-to-list
     'org-preview-latex-process-alist
     '(xelatex-imagemagick
       :programs ("xelatex" "convert")
       :message "you need to install the programs: xelatex and imagemagick."
       :image-input-type "pdf"
       :image-output-type "png"
       :image-size-adjust (0.85 . 0.85)
       :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
       :image-converter ("convert -density 200 -trim -antialias %f -quality 100 %O")))))

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

(defun +cf--select-latex-preview-backend ()
  "Pick an installed Org LaTeX preview backend for this buffer.
RivenEmacs loads fontspec globally, which requires xelatex/lualatex, so the
pdflatex-based backends (imagemagick/dvipng) crash.  Prefer our custom
`xelatex-imagemagick' backend (xelatex + imagemagick convert), which handles
fontspec and only needs the commonly-installed xelatex + convert.  Buffer-local.

Also sets `org-format-latex-options' :scale to match the current face size so
the rendered math sits at the same height as surrounding text (the default 1.0
renders far too small on hi-dpi displays), and raises the convert density for
crisp output."
  (when (boundp 'org-preview-latex-process-alist)
    (let* ((backend-programs
            '((xelatex-imagemagick "xelatex" "convert")
              (dvisvgm            "xelatex" "dvisvgm")
              (dvipng             "latex" "dvipng")))
           (prefer
            (cl-find-if
             (lambda (b)
               (let ((req (assq b backend-programs)))
                 (and req (cl-every #'executable-find (cdr req)))))
             '(xelatex-imagemagick dvisvgm dvipng))))
      (when prefer
        (setq-local org-preview-latex-default-process prefer))))
  ;; Scale the rendered math to the window's font size.  Org renders the LaTeX
  ;; at 10pt then images it; :scale multiplies that.  Match the live `default'
  ;; face height (which on hi-dpi is ~150-180), halved for a tighter inline fit.
  (let* ((face-height (face-attribute 'default :height nil t))
         (scale (cond
                 ((integerp face-height) (max 0.75 (/ face-height 180.0)))
                 (t 0.9))))
    (setq-local org-format-latex-options
                (plist-put (copy-tree org-format-latex-options)
                           :scale scale))))

(defun +cf--write-buffer (problem &optional statement-org)
  "Render PROBLEM into the current Org buffer.
STATEMENT-ORG is the full statement as Org text (from the agent); if nil or
failed, a fallback note with a browser-open pointer is shown instead."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (+cf--render-header problem))
    (insert (+cf--render-tags problem))
    (insert "** Statement\n")
    (if (and statement-org (not (string-empty-p statement-org)))
        (insert statement-org)
      (insert "Could not fetch the full statement (network/agent error).\n")
      (insert "Press =o= to open it in the browser: ")
      (insert (+cf--problem-url problem))
      (insert "\n"))
    (insert "\n")
    (insert (+cf--render-status nil))
    ;; Render statement images inline (Codeforces figures become [[url][url]]).
    (when (fboundp 'org-display-inline-images)
      (org-display-inline-images))
    ;; Preview inline math ($...$) so the $ delimiters don't show as text.
    ;; Choose an installed backend first (Org defaults to dvipng, often missing),
    ;; then call the version-appropriate preview function.
    (+cf--select-latex-preview-backend)
    (cond
     ((fboundp 'org-latex-preview)
      (condition-case err
          (org-latex-preview '(16))
        (error (message "latex preview error: %s" (error-message-string err)))))
     ((fboundp 'org-preview-latex-fragment)
      (condition-case err
          (org-preview-latex-fragment)
        (error (message "latex preview error: %s" (error-message-string err))))))))

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

(defun +cf--poll-once (contest-id submission-id)
  "Poll once: fetch submission CONTEST-ID/SUBMISSION-ID verdict.
Update the status section; stop when a terminal verdict or poll-timeout."
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
  "Start polling submission CONTEST-ID/SUBMISSION-ID.
Polls every `codeforces-poll-interval' seconds until a terminal verdict or
`codeforces-poll-timeout' seconds elapse."
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
    ;; Note the chosen language in the echo area; the statement stays in place.
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

(defun codeforces-refresh-statement ()
  "Re-fetch the current problem's statement from the site and re-render."
  (interactive)
  (unless +cf--current-problem
    (error "No problem in this buffer"))
  (let* ((problem +cf--current-problem)
         (statement (progn
                      (message "Refreshing statement for %s..." (+cf--problem-id problem))
                      (condition-case err
                          (+cf-agent-fetch-statement problem)
                        (error
                         (message "Refresh failed: %s" (error-message-string err))
                         nil)))))
    (if statement
        (progn (+cf--write-buffer problem statement)
               (message "Statement refreshed."))
      (message "Could not refresh statement."))))

(defun codeforces-poll-latest ()
  "Poll the latest submission for the current problem and show its status.
Useful to check a verdict without (re)submitting.  Reads your handle from
the stored credentials."
  (interactive)
  (unless +cf--current-problem
    (error "No problem in this buffer"))
  (let* ((problem +cf--current-problem)
         (handle (+cf--handle)))
    (unless handle
      (error "Not logged in.  Run M-x codeforces-login"))
    (let ((sub (condition-case nil
                  (+cf-find-latest-problem-submission
                   handle
                   (plist-get problem :contestId)
                   (plist-get problem :index))
                (error nil))))
      (if sub
          (let ((text (+cf--format-verdict sub)))
            (+cf--update-status text)
            (message "Latest: %s" text))
        (+cf--update-status "No submission found for this problem.")
        (message "No submission found.")))))

(defun codeforces-submit ()
  "Submit the current problem's solution via the agent and poll the verdict.
The Python agent (curl-cffi) POSTs the solution using the stored cookie;
on success it returns the new submission id, which Emacs uses to poll the
public API for a real-time verdict."
  (interactive)
  (unless +cf--current-problem
    (error "No problem in this buffer"))
  (let* ((problem +cf--current-problem)
         (sol (+cf--solution-file problem)))
    (unless sol
      (error "Run C-c C-c to start solving first"))
    (unless +cf--solution-language
      (error "Language not set"))
    (unless (+cf--handle)
      (error "Not logged in.  Run M-x codeforces-login"))
    (unless (file-exists-p sol)
      (error "Solution file not found: %s" sol))
    (+cf--update-status "Submitting via agent…")
    (condition-case err
        (let ((sub-id (+cf-agent-submit problem +cf--solution-language sol)))
          (if (and sub-id (string-match-p "^[0-9]+$" sub-id))
              (progn
                (+cf--update-status (format "Submitted as %s. Polling…" sub-id))
                (+cf--start-poll (plist-get problem :contestId) sub-id))
            ;; Agent returned non-numeric (e.g. "OK" fallback) — can't poll by id.
            (+cf--update-status
             (format "Submitted (%s). Poll the website for the verdict."
                     (or sub-id "unknown")))))
      (error
       (+cf--update-status (format "Submit failed: %s"
                                   (error-message-string err)))
       ;; Offer the browser as a fallback on agent failure.
       (when (y-or-n-p "Open the submit page in the browser instead? ")
         (+cf-submit-via-browser problem))))))

;;;###autoload
(defun codeforces-open-problem (problem)
  "Open PROBLEM (plist) in a `codeforces-problem-mode' buffer on the right.
Fetches the full statement via the Python agent; on agent failure, shows a
fallback note with a browser-open pointer (key `o')."
  (interactive)
  (let ((buf (get-buffer-create (+cf--buffer-name problem))))
    (with-current-buffer buf
      (unless (derived-mode-p 'codeforces-problem-mode)
        (codeforces-problem-mode)
        (setq buffer-read-only t))
      (setq +cf--current-problem problem)
      (message "Fetching statement for %s..." (+cf--problem-id problem))
      (let ((statement (condition-case err
                           (+cf-agent-fetch-statement problem)
                         (error
                          (message "Statement fetch failed: %s"
                                   (error-message-string err))
                          nil))))
        (+cf--write-buffer problem statement)))
    (let ((win (display-buffer buf
                               '((display-buffer-in-direction)
                                 (direction . right)
                                 (window-width . 0.5)
                                 (window . selected)))))
      ;; Select the new window so the cursor lands in the detail buffer.
      (when (windowp win)
        (select-window win)))
    buf))

(provide 'emacs-codeforces-detail)

;;; emacs-codeforces-detail.el ends here
