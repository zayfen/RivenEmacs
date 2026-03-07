;; -*- coding: utf-8; lexical-binding: t -*-

;;; init-gpt-helper.el --- gptel helper compatibility -*- lexical-binding: t; -*-

;;; Commentary:
;; Provide fallback commands so AI keybindings keep working even if
;; optional gptel extension packages are not installed.

;;; Code:

(require 'subr-x)

(declare-function gptel "gptel" (&optional arg))
(declare-function gptel-send "gptel" (&optional arg))
(declare-function gptel-request "gptel" (prompt &rest args))
(declare-function vc-git-root "vc-git" (dir))

(defcustom rivenEmacs-gptel-max-input-chars 12000
  "Maximum document size sent by helper fallbacks."
  :type 'integer
  :group 'rivenEmacs)

(defvar-local riven/gptel-review-origin-buffer nil
  "Original buffer associated with current review buffer.")

(defvar-local riven/gptel-review-start-marker nil
  "Start marker of original selected region.")

(defvar-local riven/gptel-review-end-marker nil
  "End marker of original selected region.")

(defun riven/gptel--trim-input (text)
  "Trim TEXT to `rivenEmacs-gptel-max-input-chars`."
  (if (> (length text) rivenEmacs-gptel-max-input-chars)
      (concat (substring text 0 rivenEmacs-gptel-max-input-chars)
              "\n\n[Input truncated by RivenEmacs gpt helper]")
    text))

(defun riven/gptel--region-or-buffer-text ()
  "Return selected text, or current buffer text when no region is active."
  (riven/gptel--trim-input
   (if (use-region-p)
       (buffer-substring-no-properties (region-beginning) (region-end))
     (buffer-substring-no-properties (point-min) (point-max)))))

(defun riven/gptel--send-prompt (prompt)
  "Send PROMPT with gptel, preferring `gptel-request` when available."
  (cond
   ((fboundp 'gptel-request)
    (gptel-request prompt))
   ((fboundp 'gptel-send)
    (unless (derived-mode-p 'gptel-mode)
      (call-interactively #'gptel))
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n\n"))
    (insert prompt)
    (call-interactively #'gptel-send))
   (t
    (user-error "gptel is unavailable; install gptel first"))))

(defun riven/gptel--ensure-request-api ()
  "Ensure `gptel-request` is available."
  (or (fboundp 'gptel-request)
      (progn
        (require 'gptel-request nil t)
        (fboundp 'gptel-request))
      (progn
        (require 'gptel nil t)
        (fboundp 'gptel-request))))

(defun riven/gptel--review-content ()
  "Return current review buffer content as plain string."
  (string-trim-right (buffer-substring-no-properties (point-min) (point-max))))

(defun riven/gptel-review-quit ()
  "Close current gptel review buffer."
  (interactive)
  (let ((buf (current-buffer))
        (win (get-buffer-window (current-buffer))))
    (when (window-live-p win)
      (delete-window win))
    (kill-buffer buf)))

(defun riven/gptel-review-accept-insert ()
  "Accept review result and insert below selected region in original buffer."
  (interactive)
  (unless (and (buffer-live-p riven/gptel-review-origin-buffer)
               (markerp riven/gptel-review-end-marker))
    (user-error "Original buffer or region markers are not available"))
  (let ((content (riven/gptel--review-content)))
    (when (string-empty-p content)
      (user-error "Review buffer is empty"))
    (with-current-buffer riven/gptel-review-origin-buffer
      (save-excursion
        (goto-char riven/gptel-review-end-marker)
        (unless (or (bolp)
                    (eq (char-before) ?\n))
          (insert "\n"))
        (insert content)
        (unless (eq (char-before) ?\n)
          (insert "\n"))))
    (message "Inserted generated content at selected region bottom")
    (riven/gptel-review-quit)))

(defun riven/gptel-review-accept-replace ()
  "Accept review result and replace selected region in original buffer."
  (interactive)
  (unless (and (buffer-live-p riven/gptel-review-origin-buffer)
               (markerp riven/gptel-review-start-marker)
               (markerp riven/gptel-review-end-marker))
    (user-error "Original buffer or region markers are not available"))
  (let ((content (riven/gptel--review-content)))
    (when (string-empty-p content)
      (user-error "Review buffer is empty"))
    (with-current-buffer riven/gptel-review-origin-buffer
      (save-excursion
        (goto-char riven/gptel-review-start-marker)
        (delete-region riven/gptel-review-start-marker riven/gptel-review-end-marker)
        (insert content)))
    (message "Replaced selected region with generated content")
    (riven/gptel-review-quit)))

(defvar riven/gptel-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'riven/gptel-review-quit)
    (define-key map (kbd "y") #'riven/gptel-review-accept-insert)
    (define-key map (kbd "r") #'riven/gptel-review-accept-replace)
    map)
  "Keymap for `riven/gptel-review-mode`.")

(define-derived-mode riven/gptel-review-mode special-mode "GPT-Review"
  "Major mode for reviewing generated content before applying.

Key bindings:
- `q`: close review buffer
- `y`: insert content below selected region in original buffer
- `r`: replace selected region in original buffer")

(defun riven/gptel--show-review-buffer (buffer-name content origin start end)
  "Show generated CONTENT in review BUFFER-NAME with ORIGIN and region START/END."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min))
        (riven/gptel-review-mode)
        (setq-local riven/gptel-review-origin-buffer origin)
        (setq-local riven/gptel-review-start-marker start)
        (setq-local riven/gptel-review-end-marker end)))
    (display-buffer
     buf
     '((display-buffer-in-side-window)
       (side . right)
       (slot . 0)
       (window-width . 0.3)))))

(defun riven/gptel--request-review (prompt buffer-name origin start end)
  "Send PROMPT and show response in side review BUFFER-NAME.
ORIGIN, START and END identify the original region to apply result."
  (unless (riven/gptel--ensure-request-api)
    (user-error "gptel-request is unavailable; ensure gptel package is installed"))
  (let ((loading-buf (get-buffer-create buffer-name)))
    (with-current-buffer loading-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Waiting for model response...\n\n")
        (insert "q: close    y: insert below selection    r: replace selection")
        (goto-char (point-min))
        (riven/gptel-review-mode)
        (setq-local riven/gptel-review-origin-buffer origin)
        (setq-local riven/gptel-review-start-marker start)
        (setq-local riven/gptel-review-end-marker end)))
    (display-buffer
     loading-buf
     '((display-buffer-in-side-window)
       (side . right)
       (slot . 0)
       (window-width . 0.3))))
  (gptel-request
   prompt
   :stream nil
   :callback
   (lambda (response info)
     (cond
      ((stringp response)
       (riven/gptel--show-review-buffer
        buffer-name
        response
        origin
        start
        end))
      ((eq response 'abort)
       (message "[gptel-review] request aborted"))
      ((null response)
       (message "[gptel-review] request failed: %s"
                (or (plist-get info :status) "unknown error")))
      (t
       (message "[gptel-review] unexpected response: %s" response))))))

(defun riven/gptel-translate-region-review (target-language)
  "Translate selected region into TARGET-LANGUAGE and open review buffer."
  (interactive "sTranslate to language (e.g. en, zh-CN, ja): ")
  (unless (use-region-p)
    (user-error "Please select a region first"))
  (let* ((origin (current-buffer))
         (start (copy-marker (region-beginning)))
         (end (copy-marker (region-end) t))
         (text (buffer-substring-no-properties start end))
         (prompt (format "Translate the text to %s.\nKeep meaning accurate and tone natural.\n\nText:\n%s"
                         target-language
                         (riven/gptel--trim-input text))))
    (riven/gptel--request-review
     prompt
     "*GPT Translation Review*"
     origin
     start
     end)))

(defun riven/gptel-summarize-region-review ()
  "Summarize selected region and open review buffer."
  (interactive)
  (unless (use-region-p)
    (user-error "Please select a region first"))
  (let* ((origin (current-buffer))
         (start (copy-marker (region-beginning)))
         (end (copy-marker (region-end) t))
         (text (buffer-substring-no-properties start end))
         (prompt (format "Summarize the following content in concise bullet points.\n\nContent:\n%s"
                         (riven/gptel--trim-input text))))
    (riven/gptel--request-review
     prompt
     "*GPT Summary Review*"
     origin
     start
     end)))

(defun riven/gptel--git-diff (cached)
  "Return git diff text for current repo.
When CACHED is non-nil, return staged diff."
  (let* ((repo-root (or (ignore-errors (vc-git-root default-directory))
                        (locate-dominating-file default-directory ".git"))))
    (when repo-root
      (with-temp-buffer
        (let ((default-directory repo-root))
          (when (zerop (if cached
                           (call-process "git" nil t nil "diff" "--cached")
                         (call-process "git" nil t nil "diff")))
            (string-trim (buffer-string))))))))

(unless (fboundp 'gptel-translate-region)
  (defun gptel-translate-region (target-language)
    "Translate region (or prompted text) into TARGET-LANGUAGE."
    (interactive "sTranslate to language (e.g. en, zh-CN, ja): ")
    (let* ((text (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (read-string "Text to translate: ")))
           (prompt (format "Translate the text to %s.\nKeep meaning accurate and tone natural.\n\nText:\n%s"
                           target-language
                           (riven/gptel--trim-input text))))
      (riven/gptel--send-prompt prompt))))

(unless (fboundp 'gptel-extensions-ask-document)
  (defun gptel-extensions-ask-document (question)
    "Ask QUESTION about current region or current buffer document."
    (interactive "sAsk about current document: ")
    (riven/gptel--send-prompt
     (format "Answer the question based on the provided document.\n\nQuestion:\n%s\n\nDocument:\n%s"
             question
             (riven/gptel--region-or-buffer-text)))))

(unless (fboundp 'gptel-rewrite-article)
  (defun gptel-rewrite-article ()
    "Rewrite current region or buffer into a cleaner article."
    (interactive)
    (riven/gptel--send-prompt
     (format "Rewrite the following content as a clear, concise article.\n\nContent:\n%s"
             (riven/gptel--region-or-buffer-text)))))

(unless (fboundp 'gptel-summarize-document)
  (defun gptel-summarize-document ()
    "Summarize current region or buffer document."
    (interactive)
    (riven/gptel--send-prompt
     (format "Summarize the following document in bullet points.\n\nDocument:\n%s"
             (riven/gptel--region-or-buffer-text)))))

(unless (fboundp 'gptel-query-devdoc)
  (defun gptel-query-devdoc (question)
    "Ask QUESTION as a development-oriented query."
    (interactive
     (list (read-string "Dev question: " (or (thing-at-point 'symbol t) ""))))
    (riven/gptel--send-prompt
     (format "Answer this software development question with practical examples:\n%s"
             question))))

(unless (fboundp 'gptel-generate-commit-message)
  (defun gptel-generate-commit-message ()
    "Generate a commit message from current git diff."
    (interactive)
    (let* ((staged (or (riven/gptel--git-diff t) ""))
           (unstaged (or (riven/gptel--git-diff nil) ""))
           (diff (if (string-empty-p staged) unstaged staged)))
      (if (string-empty-p diff)
          (user-error "No git diff found in current repository")
        (riven/gptel--send-prompt
         (format (concat "Generate a concise Conventional Commit message for this diff.\n"
                         "Return only the commit message.\n\nDiff:\n%s")
                 (riven/gptel--trim-input diff)))))))

(provide 'init-gpt-helper)

;;; init-gpt-helper.el ends here
