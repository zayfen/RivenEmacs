;;; lsp-bridge-popup-documentation-fix.el --- Fix for lsp-bridge popup documentation not closing properly -*- lexical-binding: t; -*-

;; 修复 lsp-bridge-popup-documentation 弹窗无法关闭的问题
;; 主要问题：移动光标后弹窗应该自动关闭，但偶现不关闭
;; 根本原因：
;; 1. 弹窗隐藏逻辑依赖于 lsp-bridge-has-lsp-server-p 的返回值
;; 2. 框架可见性检查可能失败
;; 3. 异步操作的竞态条件

(defun lsp-bridge-hide-doc-tooltip-fixed ()
  "增强版的文档弹窗隐藏函数，确保弹窗能够正确关闭。"
  (interactive)
  (when (frame-live-p lsp-bridge-popup-documentation-frame)
    ;; 强制隐藏框架，不管当前可见性状态
    (make-frame-invisible lsp-bridge-popup-documentation-frame)
    ;; 如果框架仍然存在，尝试删除它
    (when (frame-live-p lsp-bridge-popup-documentation-frame)
      (delete-frame lsp-bridge-popup-documentation-frame)
      (setq lsp-bridge-popup-documentation-frame nil))))

(defun lsp-bridge-popup-documentation--callback-fixed (value)
  "增强版的文档弹窗回调函数，确保正确处理弹窗创建和清理。"
  (let ((emacs-frame (or acm-frame--emacs-frame (selected-frame))))
    ;; 先清理可能存在的旧弹窗
    (lsp-bridge-hide-doc-tooltip-fixed)

    (with-current-buffer (get-buffer-create lsp-bridge-popup-documentation-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (insert value)
      (lsp-bridge-replace-html-entities)
      (setq-local truncate-lines nil)
      (acm-markdown-render-content))

    ;; 创建新弹窗
    (acm-frame-new lsp-bridge-popup-documentation-frame
                   lsp-bridge-popup-documentation-buffer
                   "lsp bridge popup documentation frame"
                   (/ (frame-width emacs-frame) 2)
                   (/ (frame-height emacs-frame) 2)
                   )))

(defun lsp-bridge-monitor-post-command-fixed ()
  "增强版的post-command监控函数，修复弹窗隐藏逻辑。"
  (setq-local lsp-bridge-cursor-after-command (point))

  (let ((this-command-string (format "%s" this-command)))
    (when (and lsp-bridge-mode
               (member this-command-string '("self-insert-command" "org-self-insert-command" "lsp-bridge-popup-complete-menu")))
      (lsp-bridge-try-completion))

    ;; 关键修复：将弹窗隐藏逻辑移到 lsp-bridge-has-lsp-server-p 检查之外
    ;; 这样即使LSP服务器不可用，弹窗也能正确关闭
    (unless (string-prefix-p "lsp-bridge-popup-documentation-scroll" this-command-string)
      (lsp-bridge-hide-doc-tooltip-fixed))

    (when (lsp-bridge-has-lsp-server-p)
      (unless (equal lsp-bridge-last-cursor-position
                     (setq-local lsp-bridge-last-cursor-position (point)))
        ;; Only show hover when cursor move.
        (when (and lsp-bridge-enable-hover-diagnostic
                   (not (member this-command-string
                                '("self-insert-command" "org-self-insert-command"
                                  "lsp-bridge-diagnostic-jump-next" "lsp-bridge-diagnostic-jump-prev"))))
          (lsp-bridge-diagnostic-maybe-display-error-at-point))

        ;; Only send `change_cursor' request when user change cursor, except cause by mouse wheel.
        (unless (eq last-command 'mwheel-scroll)
          (lsp-bridge-call-file-api "change_cursor" (lsp-bridge--position))
          (if (and lsp-bridge-symbols-enable-which-func
                   (featurep 'which-func) which-function-mode)
              (lsp-bridge-call-file-api "document_symbol" (lsp-bridge--position)))))

      ;; Hide diagnostic tooltip.
      (unless (member this-command-string '("lsp-bridge-diagnostic-jump-next"
                                            "lsp-bridge-diagnostic-jump-prev"))
        (lsp-bridge-diagnostic-hide-tooltip))

      ;; Hide signature tooltip.
      (lsp-bridge-hide-signature-tooltip)

      ;; Hide code action frame when Emacs got focus.
      (unless (string-prefix-p "lsp-bridge-code-action" this-command-string)
        (unless (member this-command-string '("handle-switch-frame"))
          (ignore-errors
            (lsp-bridge-code-action-popup-quit))))

      ;; Try send inlay hint if window scroll.
      (lsp-bridge-inlay-hint-monitor-window-scroll))))

(defun lsp-bridge-monitor-window-buffer-change-fixed ()
  "增强版的窗口缓冲区变化监控函数。"
  ;; 总是尝试隐藏弹窗，不管缓冲区是否改变
  (lsp-bridge-hide-doc-tooltip-fixed)
  (lsp-bridge-diagnostic-hide-tooltip)
  (lsp-bridge-hide-signature-tooltip)

  (unless (or (minibufferp)
              (string-equal (buffer-name) "*Messages*"))
    (setq lsp-bridge--last-buffer (current-buffer))))

;; 添加一个定时器来定期检查和清理孤立的弹窗
(defvar lsp-bridge-popup-cleanup-timer nil)

(defun lsp-bridge-cleanup-orphaned-popups ()
  "定期清理孤立的弹窗。"
  (when (and lsp-bridge-popup-documentation-frame
             (frame-live-p lsp-bridge-popup-documentation-frame)
             (frame-visible-p lsp-bridge-popup-documentation-frame))
    ;; 检查弹窗是否应该被隐藏（比如光标已经移动）
    (let ((current-point (point))
          (last-cursor-pos lsp-bridge-last-cursor-position))
      (when (and last-cursor-pos
                 (listp last-cursor-pos)
                 (>= (length last-cursor-pos) 3)
                 (not (equal current-point (nth 2 last-cursor-pos))))
        ;; 光标已经移动，但弹窗仍然可见，强制隐藏
        (lsp-bridge-hide-doc-tooltip-fixed)))))

;; 添加一个手动关闭弹窗的命令
(defun lsp-bridge-force-hide-documentation ()
  "强制隐藏文档弹窗。"
  (interactive)
  (lsp-bridge-hide-doc-tooltip-fixed)
  (message "文档弹窗已强制关闭"))

;; 应用修复的函数替换
(defun lsp-bridge-apply-popup-documentation-fix ()
  "应用弹窗文档修复补丁。"
  (interactive)

  ;; 替换原始函数
  (fset 'lsp-bridge-hide-doc-tooltip 'lsp-bridge-hide-doc-tooltip-fixed)
  (fset 'lsp-bridge-popup-documentation--callback 'lsp-bridge-popup-documentation--callback-fixed)
  (fset 'lsp-bridge-monitor-post-command 'lsp-bridge-monitor-post-command-fixed)
  (fset 'lsp-bridge-monitor-window-buffer-change 'lsp-bridge-monitor-window-buffer-change-fixed)

  ;; 启动清理定时器
  (when lsp-bridge-popup-cleanup-timer
    (cancel-timer lsp-bridge-popup-cleanup-timer))
  (setq lsp-bridge-popup-cleanup-timer
        (run-with-timer 1 1 'lsp-bridge-cleanup-orphaned-popups))

  ;; 添加手动关闭命令的键绑定
  (global-set-key (kbd "C-c C-d") 'lsp-bridge-force-hide-documentation)

  (message "lsp-bridge 弹窗文档修复已应用"))

;; 恢复原始函数的函数
(defun lsp-bridge-restore-original-functions ()
  "恢复原始的lsp-bridge函数。"
  (interactive)

  ;; 停止清理定时器
  (when lsp-bridge-popup-cleanup-timer
    (cancel-timer lsp-bridge-popup-cleanup-timer)
    (setq lsp-bridge-popup-cleanup-timer nil))

  ;; 这里需要重新加载原始的lsp-bridge.el来恢复函数
  (load-library "lsp-bridge")

  ;; 移除手动关闭命令的键绑定
  (global-unset-key (kbd "C-c C-d"))

  (message "已恢复原始的lsp-bridge函数"))

;; 添加调试函数
(defun lsp-bridge-debug-popup-state ()
  "调试弹窗状态。"
  (interactive)
  (message "=== 弹窗状态调试 ===")
  (message "弹窗框架: %s" lsp-bridge-popup-documentation-frame)
  (message "框架存活: %s" (frame-live-p lsp-bridge-popup-documentation-frame))
  (message "框架可见: %s" (and (frame-live-p lsp-bridge-popup-documentation-frame)
                               (frame-visible-p lsp-bridge-popup-documentation-frame)))
  (message "LSP服务器可用: %s" (lsp-bridge-has-lsp-server-p))
  (message "当前光标位置: %s" (point))
  (message "上次光标位置: %s" lsp-bridge-last-cursor-position)
  (message "当前命令: %s" this-command))

(provide 'lsp-bridge-popup-documentation-fix)
