;;; Package -- Summary: -*- coding: utf-8; lexical-binding: t -*-
;;; init-gpt.el --- config gpt

;;; Commentary:
;;; gpt config

;;; Code:
(use-package gptel
  :vc (:fetcher github :repo karthink/gptel)
  :config
  ;; OPTIONAL configuration
  (setq gptel-model   "deepseek-chat"
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
  )



(use-package gpt-extensions.el
  :vc (:fetcher github :repo kamushadenes/gptel-extensions.el)
  :bind (("C-x =" . 'gptel-extensions-refactor)))




(provide 'init-gpt)

;;; init-gpt.el ends here
