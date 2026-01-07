;;; Package -- Summary: -*- coding: utf-8; lexical-binding: t -*-
;;; init-gpt-helper.el --- GPTel helper commands

;;; Commentary:
;;; GPTel 辅助命令集合，提供代码分析、优化、生成等实用功能

;;; Code:

(require 'gptel)

;; ============================================================
;; 辅助函数
;; ============================================================

(defun gptel-helper--get-code-and-language ()
  "获取选中的代码和语言类型."
  (when (region-active-p)
    (let ((code (buffer-substring-no-properties (region-beginning) (region-end)))
          (lang (or (and (boundp 'treesit-language-at-point)
                        (treesit-language-at-point (point)))
                   (symbol-name major-mode))))
      (cons code lang))))

(defun gptel-helper--require-region (func-name)
  "检查是否选中了区域，未选中时提示."
  (unless (region-active-p)
    (user-error "请先选中要%s的代码" func-name)))

;; ============================================================
;; 代码理解类命令
;; ============================================================

(defun gptel-explain-code ()
  "解释选中的代码，分析其功能、逻辑和关键点."
  (interactive)
  (gptel-helper--require-region "解释")
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (gptel-request 
     (format "请详细解释以下代码的功能、逻辑和关键点：\n\n```\n%s\n```" code)
     :buffer (current-buffer))))

(defun gptel-ask-about-code (question)
  "询问关于选中代码的问题."
  (interactive "s关于这段代码的问题: ")
  (gptel-helper--require-region "询问")
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (gptel-request 
     (format "代码：\n```\n%s\n```\n\n问题：%s" code question)
     :buffer (current-buffer))))

(defun gptel-generate-docstring ()
  "为选中的函数生成规范的文档字符串."
  (interactive)
  (gptel-helper--require-region "生成文档")
  (let* ((result (gptel-helper--get-code-and-language))
         (code (car result))
         (lang (cdr result)))
    (gptel-request 
     (format "请为以下 %s 函数生成规范的文档字符串（docstring），包括功能描述、参数说明、返回值和示例：\n\n```\n%s\n```" lang code)
     :buffer (current-buffer))))

;; ============================================================
;; 代码改进类命令
;; ============================================================

(defun gptel-optimize-code ()
  "优化选中的代码，提高性能和可读性."
  (interactive)
  (gptel-helper--require-region "优化")
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (gptel-request 
     (format "请优化以下代码，提高性能、可读性和代码质量。说明优化点并提供优化后的代码：\n\n```\n%s\n```" code)
     :buffer (current-buffer))))

(defun gptel-fix-code ()
  "修复选中代码中的问题和错误."
  (interactive)
  (gptel-helper--require-region "修复")
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (gptel-request 
     (format "请检查并修复以下代码中的错误、潜在问题和不良实践。提供修复后的代码并说明修改原因：\n\n```\n%s\n```" code)
     :buffer (current-buffer))))

(defun gptel-simplify-code ()
  "简化选中的代码，使其更简洁易读."
  (interactive)
  (gptel-helper--require-region "简化")
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (gptel-request 
     (format "请简化以下代码，使其更简洁易读，同时保持功能不变：\n\n```\n%s\n```" code)
     :buffer (current-buffer))))

(defun gptel-suggest-refactor ()
  "提供详细的代码重构建议."
  (interactive)
  (gptel-helper--require-region "分析重构")
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (gptel-request 
     (format "请分析以下代码，提供详细的重构建议，包括：\n1. 代码结构改进\n2. 设计模式应用\n3. 命名优化\n4. 函数拆分建议\n\n```\n%s\n```" code)
     :buffer (current-buffer))))

;; ============================================================
;; 代码生成类命令
;; ============================================================

(defun gptel-add-comments ()
  "为选中的代码添加详细注释."
  (interactive)
  (gptel-helper--require-region "添加注释")
  (let* ((result (gptel-helper--get-code-and-language))
         (code (car result))
         (lang (cdr result)))
    (gptel-request 
     (format "请为以下 %s 代码添加清晰的注释，保持原代码格式，只输出带注释的完整代码：\n\n```\n%s\n```" lang code)
     :buffer (current-buffer))))

(defun gptel-generate-test ()
  "为选中的代码生成单元测试."
  (interactive)
  (gptel-helper--require-region "生成测试")
  (let* ((result (gptel-helper--get-code-and-language))
         (code (car result))
         (lang (cdr result)))
    (gptel-request 
     (format "请为以下 %s 代码生成完整的单元测试，包括边界情况和异常处理：\n\n```\n%s\n```" lang code)
     :buffer (current-buffer))))

(defun gptel-complete-code ()
  "根据上下文补全或续写代码."
  (interactive)
  (let* ((start (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point)))
         (code (buffer-substring-no-properties start end)))
    (gptel-request 
     (format "请根据以下代码上下文，补全或续写代码。只输出补全的代码部分：\n\n```\n%s\n```\n\n请继续..." code)
     :buffer (current-buffer))))

;; ============================================================
;; 代码质量类命令
;; ============================================================

(defun gptel-review-code ()
  "对选中的代码进行全面的代码审查."
  (interactive)
  (gptel-helper--require-region "审查")
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (gptel-request 
     (format "请对以下代码进行全面的代码审查，包括：\n1. 代码质量和最佳实践\n2. 潜在的bug和安全问题\n3. 性能优化建议\n4. 可维护性和可读性\n\n```\n%s\n```" code)
     :buffer (current-buffer))))

(defun gptel-check-security ()
  "检查选中代码的安全问题."
  (interactive)
  (gptel-helper--require-region "检查安全性")
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (gptel-request 
     (format "请分析以下代码的安全性，指出潜在的安全漏洞和风险，并提供修复建议：\n\n```\n%s\n```" code)
     :buffer (current-buffer))))

;; ============================================================
;; 其他实用功能
;; ============================================================

(defun gptel-convert-style (style)
  "将选中代码转换为指定的代码风格（如 functional, OOP, async）."
  (interactive "sTarget style (e.g., functional, OOP, async): ")
  (gptel-helper--require-region "转换风格")
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (gptel-request 
     (format "请将以下代码转换为 %s 风格，保持功能不变：\n\n```\n%s\n```" style code)
     :buffer (current-buffer))))

(defun gptel-explain-error (error-msg)
  "解释错误信息并提供详细的解决方案."
  (interactive "sError message: ")
  (gptel-request 
   (format "请解释以下错误信息，分析可能的原因，并提供详细的解决方案：\n\n%s" error-msg)
   :buffer (current-buffer)))

(provide 'init-gpt-helper)

;;; init-gpt-helper.el ends here

