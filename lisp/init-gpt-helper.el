;; -*- coding: utf-8; lexical-binding: t -*-

;;; init-gpt-helper.el --- GPTel helper commands

;;; Commentary:
;; GPTel 辅助命令集合，提供代码分析、优化、生成等实用功能

;;; Code:

(require 'gptel)
(require 'url)

;; ============================================================
;; 公用临时Buffer框架
;; ============================================================

(defvar gptel-temp-original-buffer nil
  "The original buffer where the command was invoked.")

(defvar gptel-temp-content nil
  "The content to be displayed in the temporary buffer.")

(defvar gptel-temp-refine-prompt ""
  "The additional prompt for refining the content.")

(defvar gptel-temp-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "y") #'gptel-temp-buffer-accept)
    (define-key map (kbd "q") #'gptel-temp-buffer-quit)
    (define-key map (kbd "C-c C-c") #'gptel-temp-buffer-refine)
    map)
  "Keymap for temporary GPT buffer.")

(defun gptel-create-temp-buffer (buffer-name mode-name header-text)
  "Create a temporary buffer with the given NAME, MODE, and HEADER text."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert header-text)
      (insert "\n\n")
      (insert gptel-temp-content)
      (goto-char (point-min))
      (forward-line 2)
      (funcall mode-name)
      (use-local-map gptel-temp-buffer-map)
      (setq-local gptel-temp-original-buffer (current-buffer)))
    buffer))

(defun gptel-temp-buffer-accept ()
  "Accept the content in the temporary buffer and close it.
Replace the original buffer content with the current buffer content."
  (interactive)
  (when gptel-temp-original-buffer
    (with-current-buffer gptel-temp-original-buffer
      (erase-buffer)
      (insert gptel-temp-content)))
  (kill-buffer (current-buffer)))

(defun gptel-temp-buffer-quit ()
  "Quit the temporary buffer without accepting changes."
  (interactive)
  (kill-buffer (current-buffer)))

(defun gptel-temp-buffer-refine ()
  "Refine the content based on additional user input."
  (interactive)
  (let ((prompt (read-string "Enter additional instructions for refinement: ")))
    (setq gptel-temp-refine-prompt prompt)
    (message "Refining content with additional instructions...")))

(defun gptel-temp-buffer-wait-for-refine (original-buffer prompt-text callback)
  "Wait for user refinement and retry the request.
ORIGINAL-BUFFER is the buffer to update.
PROMPT-TEXT is the base prompt.
CALLBACK is the function to call after response."
  (if (string-empty-p gptel-temp-refine-prompt)
      (funcall callback)
    (let* ((combined-prompt (format "%s\n\nAdditional instructions:\n%s"
                                    prompt-text
                                    gptel-temp-refine-prompt)))
      (gptel-request combined-prompt
        :callback (lambda (response _metadata)
                    (when response
                      (setq gptel-temp-content response)
                      (with-current-buffer "*GPT Temp*"
                        (erase-buffer)
                        (insert "=== Refined Content ===\n\n")
                        (insert gptel-temp-content)
                        (goto-char (point-min))
                        (forward-line 2))
                      (setq gptel-temp-refine-prompt "")
                      (message "Refinement complete. Press y to accept, q to quit, or C-c C-c to refine further.")))))))

;; ============================================================
;; 辅助函数
;; ============================================================

(defun gptel-get-region-or-buffer ()
  "Get content from region or buffer.
If region is active, return region content.
Otherwise, return buffer content."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-string)))

(defun gptel-get-region-or-word ()
  "Get content from region or current word.
If region is active, return region content.
Otherwise, return current word at point."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol)))

(defun gptel-read-document-input ()
  "Read document input (file path or URL) from user."
  (let ((input (read-string "Enter file path or URL: ")))
    (cond
     ((string-match-p "^https?://" input)
      (gptel-fetch-url-content input))
     ((file-exists-p input)
      (gptel-read-file-content input))
     (t
      (error "File does not exist and not a valid URL: %s" input)))))

(defun gptel-fetch-url-content (url)
  "Fetch content from URL."
  (with-temp-buffer
    (url-insert-file-contents url)
    (buffer-string)))

(defun gptel-read-file-content (file-path)
  "Read content from FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun gptel-call-gpt-with-temp-buffer (title prompt callback)
  "Call GPT with the given PROMPT and show result in temp buffer.
TITLE is the buffer name prefix.
CALLBACK is called with the response."
  (message "Processing...")
  (gptel-request prompt
    :callback (lambda (response _metadata)
                (when response
                  (setq gptel-temp-content response)
                  (let ((buffer-name (format "*GPT %s*" title)))
                    (gptel-create-temp-buffer buffer-name 'markdown-mode
                      (format "=== %s ===\nPress 'y' to accept, 'q' to quit, 'C-c C-c' to refine" title)))
                  (pop-to-buffer (format "*GPT %s*" title))
                  (message "Result ready. Press y to accept, q to quit, or C-c C-c to refine.")))))

;; ============================================================
;; 翻译功能
;; ============================================================

(defun gptel-translate-region (target-languages-str)
  "Translate the text in the active region using GPTel."
  (interactive
   (list (read-string "Target language(s) (comma-separated, default 'en'): " "en")))

  ;; 1. Source text must be from a marked region
  (unless (region-active-p)
    (error "No region active. Please mark a region to translate"))

  (let* ((original-buffer (current-buffer))
         (original-text (buffer-substring-no-properties (region-beginning) (region-end)))
         (beg (region-beginning))
         (end (region-end)))

    (let* ((lang (string-trim-left target-languages-str))
           (prompt-text
            (format "Translate the following text to %s. Preserve all formatting, including line breaks, spacing, and punctuation. Do not add any introductory or concluding remarks, or any conversational text. Just provide the translated text:\n\n%s" lang original-text))
           (translated-text nil)
           (confirmation-buffer (get-buffer-create "*Translation Confirmation*")))

      ;; Call gptel to get the translation
      (message "Translating...")
      ;; Using gptel-send-prompt synchronously for simplicity in this flow
      (gptel-request prompt-text
        :callback (lambda (response _metadata)
                    (progn
                      (setq translated-text response)
                      (if translated-text
                          (progn
                            (message "Translation succeeded")
                            (defun accept-translation ()
                              (interactive)
                              (with-current-buffer original-buffer
                                (delete-region beg end)
                                (insert translated-text)
                                (delete-window-or-kill-buffer confirmation-buffer)))

                            (defun reject-translation ()
                              (interactive)
                              (delete-window-or-kill-buffer confirmation-buffer))

                            (with-current-buffer confirmation-buffer
                              (erase-buffer)
                              (insert (format "Translated to %s:\n\n---\n%s\n---\n\n" lang translated-text))
                              (insert
                               (concat
                                (propertize "Press " 'face 'default)
                                (propertize "Enter" 'face '(:weight bold :foreground "green"))
                                (propertize " to accept and replace.\n" 'face 'default)
                                (propertize "Press " 'face 'default)
                                (propertize "C-g" 'face '(:weight bold :foreground "green"))
                                (propertize " or " 'face 'default)
                                (propertize "q" 'face '(:weight bold :foreground "green"))
                                (propertize " to reject and quit.\n\n" 'face 'default)))
                              (goto-char (point-min))
                              (local-set-key (kbd "\r") #'accept-translation)
                              (local-set-key (kbd "C-g") #'reject-translation)
                              (local-set-key (kbd "q") #'reject-translation))
                            (pop-to-buffer confirmation-buffer))
                        (message "Failed to get translation for %s." lang)
                        (when (buffer-live-p confirmation-buffer)
                          (delete-window-or-kill-buffer confirmation-buffer)))
                      )
                    )))             ; end of inner let*
      )                                 ;end of outer let*
    )

;; ============================================================
;; 重写技术文章
;; ============================================================

(defun gptel-rewrite-article ()
  "Rewrite the current buffer or region as a professional technical article."
  (interactive)
  (let* ((content (gptel-get-region-or-buffer))
         (prompt (format "Rewrite the following content into a professional technical blog article:\n1. Use Markdown format\n2. Add Hugo blog frontmatter header with title, date, tags, and categories\n3. Structure should include overview, principles, examples, and summary\n4. Maintain technical depth and professionalism\n5. Output in Simplified Chinese\n\nContent:\n%s" content)))
    (gptel-call-gpt-with-temp-buffer "Article Rewrite" prompt nil)))

;; ============================================================
;; 中文文档总结
;; ============================================================

(defun gptel-summarize-document ()
  "Summarize a document (file path or URL) in Chinese."
  (interactive)
  (let* ((content (gptel-read-document-input))
         (prompt (format "请用中文总结以下文档的主要内容，包括：\n1. 文档概述\n2. 关键概念和要点\n3. 重要细节\n\n文档内容：\n%s" content)))
    (gptel-call-gpt-with-temp-buffer "Document Summary" prompt nil)))

;; ============================================================
;; 开发文档查询
;; ============================================================

(defun gptel-query-devdoc ()
  "Generate documentation for the selected code or current word."
  (interactive)
  (let* ((content (gptel-get-region-or-word))
         (prompt (format "为以下代码生成详细的技术文档，结果使用中文显示：\n1. 函数功能说明\n2. 参数说明\n3. 返回值说明\n4. 使用示例（至少3个不同场景，用代码块展示）\n5. 注意事项和常见错误\n\n代码：\n%s" content)))
    (gptel-call-gpt-with-temp-buffer "DevDoc" prompt nil)))

;; ============================================================
;; 生成代码提交信息
;; ============================================================

(defun gptel-generate-commit-message ()
  "Generate a professional git commit message for the current changes."
  (interactive)
  (let* ((content (gptel-get-region-or-buffer))
         (prompt (format "Generate a professional git commit message for the following changes:\n1. Use English\n2. Follow Conventional Commits format (type(scope): description)\n3. Clear description with change rationale\n4. List the main changes\n\nCode changes:\n%s" content))
         (commit-message nil))
    (message "Generating commit message...")
    (gptel-request prompt
      :callback (lambda (response _metadata)
                  (when response
                    (setq commit-message response)
                    (with-current-buffer (current-buffer)
                      (insert "\n")
                      (insert commit-message)
                      (insert "\n"))
                    (message "Commit message inserted successfully!"))))))

(provide 'init-gpt-helper)

;;; init-gpt-helper.el ends here
