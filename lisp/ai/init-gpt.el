;;; Package -- Summary: -*- coding: utf-8; lexical-binding: t -*-
;;; init-gpt.el --- config gpt

(eval-and-compile
  (let ((current-file (or load-file-name byte-compile-current-file buffer-file-name)))
    (when current-file
      (add-to-list 'load-path (file-name-directory current-file)))))

;;; Commentary:
;;; GPT 配置：包括 gptel, gpt-extensions 和辅助命令

;;; Code:

(require 'init-prompt-template nil t)
(require 'init-gpt-helper nil t)

(use-package gptel
  :vc (:url "https://github.com/karthink/gptel" :branch "master")
  :commands (gptel-translate-region
             gptel-rewrite
             gptel-menu
             gptel-send
             gptel-extensions-ask-document
             gptel-rewrite-article
             gptel-summarize-document
             gptel-query-devdoc
             gptel-generate-commit-message)
  :after exec-path-from-shell
  :config
  (require 'gptel-openai-extras)
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (setq gptel-model 'deepseek-chat
        gptel-backend
        (gptel-make-deepseek "DeepSeek"
          :stream t
          :key (getenv "DEEPSEEK_API_KEY"))))

(use-package gpt-extensions
  :vc (:url "https://github.com/kamushadenes/gptel-extensions.el")
  :bind (("C-x =" . 'gptel-extensions-refactor)))

(provide 'init-gpt)
