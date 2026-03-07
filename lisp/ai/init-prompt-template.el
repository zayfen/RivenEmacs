;; -*- coding: utf-8; lexical-binding: t; -*-

;;; init-prompt-template.el --- Centralized prompt templates for GPT/Agent operations

;;; Commentary:
;; Central prompt definitions shared by GPT and agent-shell modules.

;;; Code:

(defcustom riven-prompt-explain-code
  (concat
   "Explain the following code in Chinese.\n"
   "Focus on purpose, control flow, and key edge cases.\n"
   "If there are potential bugs or risks, list them clearly.\n\n"
   "Code:\n%s")
  "Prompt template used by `riven/agent-shell-explain-code`."
  :type 'string
  :group 'rivenEmacs)

(defcustom riven-prompt-refactor-code
  (concat
   "Refactor the following code while preserving behavior.\n"
   "Return only the updated code block.\n"
   "Keep naming clear and remove duplication when practical.\n\n"
   "Code:\n%s")
  "Prompt template used by `riven/agent-shell-refactor-code`."
  :type 'string
  :group 'rivenEmacs)

(defcustom riven-prompt-add-comments
  (concat
   "Add concise, high-value comments to the following code.\n"
   "Do not change runtime behavior.\n"
   "Prefer explaining intent and non-obvious logic.\n\n"
   "Code:\n%s")
  "Prompt template used by `riven/agent-shell-add-comments`."
  :type 'string
  :group 'rivenEmacs)

(defcustom riven-prompt-fix-errors
  (concat
   "Fix the code issues described below.\n"
   "Return a corrected patch or code block.\n\n"
   "Diagnostics:\n%s")
  "Prompt template used by `riven/agent-shell-fix-errors`."
  :type 'string
  :group 'rivenEmacs)

(provide 'init-prompt-template)
