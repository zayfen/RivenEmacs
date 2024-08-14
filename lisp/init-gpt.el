;;; -*- coding: utf-8; lexical-binding: t -*-


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







(provide 'init-gpt)
