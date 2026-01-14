;;; Package -- Summary: -*- coding: utf-8; lexical-binding: t -*-
;;; init-gpt.el --- config gpt

;;; Commentary:
;;; GPT 配置：包括 gptel, gpt-extensions 和 claude-code-ide

;;; Code:

;; ============================================================
;; 辅助函数
;; ============================================================

(defun delete-window-or-kill-buffer (buffer-name)
  "Delete window if the window only contain one buffer (BUFFER-NAME), otherwise delete buffer (BUFFER-NAME)."
  (if (window-prev-buffers)
      (kill-buffer buffer-name)
    (delete-windows-on buffer-name)))

;; ============================================================
;; GPTel 配置
;; ============================================================

(use-package gptel
  :vc (:url "https://github.com/karthink/gptel")
  :commands (gptel-translate-region gptel-rewrite)
  :after exec-path-from-shell
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)

  (require 'gptel-openai-extras)
  (setq gptel-model 'deepseek-chat
        gptel-backend
        (gptel-make-deepseek "DeepSeek"
          :stream t
          :key (getenv "DEEPSEEK_API_KEY")))

  ;; 翻译功能（带确认）
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
    ))                                  ;end of defun

;; 加载 GPTel 辅助命令
(require 'init-gpt-helper)

;; ============================================================
;; GPT Extensions
;; ============================================================

(use-package gpt-extensions
  :vc (:url "https://github.com/kamushadenes/gptel-extensions.el")
  :bind (("C-x =" . 'gptel-extensions-refactor)))

;; ============================================================
;; Claude Code IDE
;; ============================================================

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("M-*" . claude-code-ide-menu)
  :config
  (setq claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-emacs-tools-setup))


;; ============================================================
;; 结束
;; ============================================================

(provide 'init-gpt)

;;; init-gpt.el ends here
