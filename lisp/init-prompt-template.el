;; -*- coding: utf-8; lexical-binding: t; -*-

;;; init-prompt-template.el --- Centralized prompt templates for GPT/Agent operations

;;; Commentary:
;; This file contains all centralized prompt templates used by init-gpt-helper.el
;; and init-agent-shell.el. Managing prompts in one place makes them easier to
;; maintain, update, and ensure consistency across all AI-assisted operations.
;;
;; Prompt variables use format-style templates where %s is replaced with dynamic
;; content (code, text, etc.) at runtime.
;;
;; Usage: (format riven-prompt-explain-code "```elisp\n(defun hello () \"World\")\n```")

;;; Code:

;; ============================================================
;; Agent Shell Prompts (init-agent-shell.el)
;; ============================================================

(defconst riven-prompt-explain-code
  "Please use Chinese to explain this code in detail:
1. What does this code do?
2. How does it work?
3. Any potential issues or improvements?

```
%s
```"
  "Prompt template for explaining code in Agent Shell.")

(defconst riven-prompt-refactor-code
  "Please refactor this code:
1. Improve readability and maintainability
2. Follow best practices
3. Add comments where needed

Provide the refactored code with explanations.

```
%s
```"
  "Prompt template for refactoring code in Agent Shell.")

(defconst riven-prompt-add-comments
  "Please add appropriate comments to this code following language best practices:
1. Add docstring for functions/classes
2. Explain complex logic with inline comments
3. Use clear and concise language

```
%s
```"
  "Prompt template for adding comments to code in Agent Shell.")

(defconst riven-prompt-fix-errors
  "Please fix this flymake error:
1. Identify the root cause
2. Provide the corrected code
3. Explain what was wrong and how you fixed it

Error: %s"
  "Prompt template for fixing flymake errors in Agent Shell.")

;; ============================================================
;; GPT Helper Prompts (init-gpt-helper.el)
;; ============================================================

(defconst gptel-prompt-translate
  "Translate the following text to %s. Preserve all formatting, including line breaks, spacing, and punctuation. Do not add any introductory or concluding remarks, or any conversational text. Just provide the translated text:

%s"
  "Prompt template for translating text. Parameters: (language original-text)")

(defconst gptel-prompt-rewrite-article
  "你是一名经验丰富的一线工程师，同时也是擅长技术写作的博客作者。

请围绕【%s】撰写一篇高质量技术博客文章，目标读者是【<读者层级：初级 / 中级 / 高级工程师>】。

### 写作目标
- 帮助读者真正\"理解\"而不是只\"知道\"
- 解释设计原理、底层机制和工程取舍
- 让读者看完后具备实际使用或设计能力

### 背景要求
- 默认读者具备：<前置知识，如 JavaScript 基础 / iOS 开发经验>
- 不回避复杂概念，但要循序渐进

### 内容要求
1. 使用真实工程视角，不写教科书式定义
2. 每个重要结论都回答：
   - 它是什么？
   - 为什么要这样设计？
   - 不这样会有什么问题？
3. 结合实际场景或踩坑经验说明
4. 必要时进行横向对比（方案 A vs B）
5. 适当加入\"误区 / 常见错误 / 注意事项\"

### 推荐文章结构
请按以下结构组织内容（可根据主题微调）：

1. 引言
   - 问题背景
   - 真实场景或痛点
   - 文章将解决什么问题

2. 问题拆解
   - 核心问题是什么
   - 为什么这个问题不简单

3. 核心原理
   - 底层机制
   - 关键概念解释
   - 必要的示意性说明（文字描述即可）

4. 设计与实现
   - 关键设计决策
   - 核心流程（步骤 / 伪代码 / 图示描述）
   - 代码示例（精简但完整）

5. 工程实践与踩坑
   - 常见错误
   - 性能 / 稳定性 / 边界问题
   - 真实经验总结

6. 延伸与对比
   - 可选方案
   - 各方案优劣与适用场景

7. 总结
   - 关键结论回顾
   - 一句话 takeaway

### 写作风格
- 技术严谨但不生硬
- 像工程师在和工程师交流
- 避免营销话术和空洞总结
- 适度使用小标题、列表、表格增强可读性

### 输出格式
- 使用 Markdown
- 标题清晰、层级分明
- 代码必须可读、可运行（如适用）

请直接输出完整文章正文，不要解释你的写作过程。

Output rules:
1. Use Markdown format
2. Add Hugo blog frontmatter header with title, date, tags, and categories
5. Output in Simplified Chinese"
  "Prompt template for rewriting content as professional technical articles.")

(defconst gptel-prompt-summarize-document
  "请用中文总结以下文档的主要内容，包括：
1. 文档概述
2. 关键概念和要点
3. 重要细节

文档内容：
%s"
  "Prompt template for summarizing documents in Chinese.")

(defconst gptel-prompt-generate-devdoc
  "为以下代码生成详细的技术文档，结果使用中文显示：
1. 函数功能说明
2. 参数说明
3. 返回值说明
4. 使用示例（至少3个不同场景，用代码块展示）
5. 注意事项和常见错误

代码：
%s"
  "Prompt template for generating development documentation.")

(defconst gptel-prompt-generate-commit
  "Generate a professional git commit message for the following changes:
1. Use English
2. Follow Conventional Commits format (type(scope): description)
3. Clear description with change rationale
4. List the main changes
5. Keep the message concise (50 chars for title, 72 chars per line for body)

Code changes:
%s"
  "Prompt template for generating git commit messages.")

;; ============================================================
;; Prompt Helper Functions
;; ============================================================

(defun riven-format-prompt (template &rest args)
  "Format a prompt TEMPLATE with ARGS.
This is a convenience wrapper around `format' for prompt templates."
  (apply #'format template args))

(provide 'init-prompt-template)

;;; init-prompt-template.el ends here
