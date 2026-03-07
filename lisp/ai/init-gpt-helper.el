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
