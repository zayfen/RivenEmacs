;;; Package -- Summary: -*- coding: utf-8; lexical-binding: t -*-
;;; init-gpt.el --- config gpt

;;; Commentary:
;;; gpt config

;;; Code:

(use-package gptel
  :vc (:fetcher github :repo karthink/gptel)
  :commands (gptel-translate-region)
  :config
  (setq gptel-model   'deepseek-chat
        gptel-backend
        (gptel-make-openai "DeepSeek"
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key (getenv "DEEPSEEK_API_KEY")
          :models '(deepseek-chat deepseek-coder)))

  ;; translator
  (defun gptel-translate-region (target-languages-str)
    "Translate the text in the active region using GPTel."
    (interactive
     (list (read-string "Target language(s) (comma-separated, default 'en'): " "en")))

    ;; 1. Source text must be from a marked region
    (unless (region-active-p)
      (error "No region active. Please mark a region to translate."))

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
                                  (delete-windows-on confirmation-buffer)))

                              (defun reject-translation ()
                                (interactive)
                                (delete-windows-on confirmation-buffer))

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
                              (delete-windows-on confirmation-buffer)))
                          )
                        )))             ; end of inner let*
      )                                 ;end of outer let*
    )                                   ;end of defun
  )



(use-package gpt-extensions.el
  :vc (:fetcher github :repo kamushadenes/gptel-extensions.el)
  :bind (("C-x =" . 'gptel-extensions-refactor)))



(use-package aidermacs
  :vc (:fetcher github :repo MatthewZMD/aidermacs)
  :bind (("M-I" . aidermacs-transient-menu))
  :config
                                        ; Enable minor mode for Aider files
  (aidermacs-setup-minor-mode)
  :custom
  (aidermacs-auto-commits nil)
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "deepseek"))

(provide 'init-gpt)

;; {Hello, World}
;; {Hello, World}

;;; init-gpt.el ends here
