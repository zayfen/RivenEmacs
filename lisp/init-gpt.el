;;; Package -- Summary: -*- coding: utf-8; lexical-binding: t -*-
;;; init-gpt.el --- config gpt

;;; Commentary:
;;; gpt config

;;; Code:
(use-package gptel
  :vc (:fetcher github :repo karthink/gptel)
  :config
  ;; OPTIONAL configuration
  (setq gptel-model   'deepseek-chat
        gptel-backend
        (gptel-make-openai "DeepSeek"
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key (getenv "DEEPSEEK_API_KEY")
          :models '(deepseek-chat deepseek-coder)))

  ;; define other commands
  (defun gptel-translate-to-langs ()
    (interactive)
    (message "Translate text to many languages...")
    (require 'gptel)
    (gptel-request
        (format "把下面的内容(自动侦测源语言)翻译成如下语言：en, jp, kr, fr, hk, it, de ， 只输出内容， 不要解释：\n %s"
                (if (region-active-p)
                    (buffer-substring-no-properties (mark) (point))
                  (substring-no-properties (buffer-string)))
                :system "你是一个翻译家， 精通如下语言互译：汉语、英语、日语、韩语、法语、繁体中文、意大利语、德语")))

  (defun gptel-translate-to-english-and-replace ()
    "Translate the selected text to English and replace it.
If no text is selected, prompt the user to select text first."
    (interactive)
    (if (not (use-region-p))  ; 检查是否有选中的文本
        (message "No text selected. Please select some text first.")
      (let ((selected-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (if (string-blank-p selected-text)  ; 检查选中的文本是否为空
            (message "Selected text is empty. Please select some text.")
          (message "Translating selected text...")  ; 提示翻译开始
          (gptel-request
              (concat "Don't explain, just translate the following text to English:\n\n" selected-text)
            :callback (lambda (response _metadata)
                        (if (string-blank-p response)  ; 检查翻译结果是否为空
                            (message "Translation failed. Please try again.")
                          (when (use-region-p)
                            (delete-region (region-beginning) (region-end))
                            (insert response)
                            (message "Translation complete!")))))))))
  )





(use-package gpt-extensions.el
  :vc (:fetcher github :repo kamushadenes/gptel-extensions.el)
  :bind (("C-x =" . 'gptel-extensions-refactor)))


(use-package gptel-aibo
  :vc (:fetcher github :repo dolmens/gptel-aibo)
  :after (gptel)
  :hook (prog-mode . gptel-aibo-complete-mode)
  ;; :config
  ;; (add-hook 'prog-mode-hook #'gptel-aibo-complete-mode)
  )

(provide 'init-gpt)

;;; init-gpt.el ends here
