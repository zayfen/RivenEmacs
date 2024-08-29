;;; init-gpt.el --- config gpt
;;; -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;; gpt config

;;; Code:
(use-package gptel
  :vc (:fetcher github :repo karthink/gptel)
  :config
  ;; OPTIONAL configuration
  (setq gptel-model   "llama-3.1-70b-versatile"
        gptel-backend
        (gptel-make-openai "Groq"
                           :host "api.groq.com"
                           :endpoint "/openai/v1/chat/completions"
                           :stream t
                           :key (getenv "GROQ_API_KEY")
                           :models '("llama-3.1-70b-versatile"
                                     "llama-3.1-8b-instant"
                                     "llama3-70b-8192"
                                     "llama3-8b-8192"
                                     "mixtral-8x7b-32768"
                                     "gemma-7b-it"))))


(use-package gpt-extensions.el
  :vc (:fetcher github :repo kamushadenes/gptel-extensions.el)
  :bind (("C-x =" . 'gptel-extensions-refactor)))




(provide 'init-gpt)

;;; init-gpt.el ends here
