;;; Package -- Summary: -*- coding: utf-8; lexical-binding: t -*-
;;; init-gpt.el --- config gpt

;;; Commentary:
;;; GPT 配置：包括 gptel, gpt-extensions 和 claude-code-ide

;;; Code:


;; ============================================================
;; GPTel 配置
;; ============================================================

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
          :key (getenv "DEEPSEEK_API_KEY")))

  ;; 加载 GPTel 辅助命令
  (require 'init-gpt-helper))


;; ============================================================
;; GPT Extensions
;; ============================================================

(use-package gpt-extensions
  :vc (:url "https://github.com/kamushadenes/gptel-extensions.el")
  :bind (("C-x =" . 'gptel-extensions-refactor)))

;; ============================================================
;; 结束
;; ============================================================

(provide 'init-gpt)

;;; init-gpt.el ends here
