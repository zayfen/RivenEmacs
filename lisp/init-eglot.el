;;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-eglot.el --- eglot LSP client configuration

;;; Commentary:
;; eglot 是 Emacs 30 内置的 LSP 客户端
;; 配置 eglot-booster 以提升性能
;; 配置 corfu 作为补全前端

;;; Code:

;; eglot 是 Emacs 30 内置的，不需要安装
(use-package eglot
  :ensure nil
  :hook ((python-mode python-ts-mode
          js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode
          rust-mode rust-ts-mode
          c-mode c-ts-mode c++-mode c++-ts-mode
          java-mode java-ts-mode
          kotlin-mode kotlin-ts-mode
          php-mode php-ts-mode
          web-mode vue-mode
          css-mode css-ts-mode
          html-mode html-ts-mode) . eglot-ensure)
  :custom
  ;; 连接超时时间（秒）
  (eglot-connect-timeout 60)
  ;; 自动补全时显示文档
  (eglot-autoshutdown t)
  ;; 服务器关闭时自动关闭
  (eglot-confirm-server-initiated-edits nil)
  ;; 不确认服务器发起的编辑
  (eglot-events-buffer-size 2000000)
  ;; 事件缓冲区大小
  :config
  ;; 添加一些常用的服务器配置
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               `((js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode) .
                 ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               `((rust-mode rust-ts-mode) . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs
               `((c-mode c-ts-mode c++-mode c++-ts-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs
               `((java-mode java-ts-mode) . ("jdtls")))
  (add-to-list 'eglot-server-programs
               `((web-mode) . ("volar-server" "--stdio"))))

;; eglot-booster: 使用 lsp-booster 提升 eglot 性能
(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :custom
  ;; 是否对远程服务器（TRAMP）启用 booster
  (eglot-booster-no-remote-boost nil)
  ;; 是否仅使用 I/O 优化（不使用字节码）
  ;; Emacs 30+ 的 JSON 解析器已经很快了，可以只使用 I/O 优化
  (eglot-booster-io-only nil)
  :config
  (eglot-booster-mode))

;; corfu: 现代化的补全前端
(use-package corfu
  :vc (:url "https://github.com/minad/corfu")
  :custom
  ;; 补全候选数量
  (corfu-max-width 80)
  (corfu-min-width 20)
  ;; 补全延迟（秒）
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  ;; 自动补全设置
  (corfu-auto t)
  ;; 补全位置：在点上方或下方
  (corfu-popupinfo-delay '(0.5 . 0.2))
  ;; 补全提示信息延迟
  :init
  (global-corfu-mode)
  :config
  ;; 在终端中禁用 corfu
  (unless (display-graphic-p)
    (corfu-mode -1))
  ;; 加载 corfu 扩展（从 extensions 子目录）
  (let ((extensions-dir (expand-file-name "extensions" (file-name-directory (locate-library "corfu")))))
    (when (file-directory-p extensions-dir)
      (add-to-list 'load-path extensions-dir))))

;; corfu-terminal: 终端中的 corfu 支持
(use-package corfu-terminal
  :vc (:url "https://github.com/minad/corfu-terminal")
  :after corfu
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode))

;; corfu-popupinfo: 显示补全项的详细信息（包含在 corfu 的 extensions 目录中）
(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  ;; 信息显示位置
  (corfu-popupinfo-position 'bottom)
  ;; 显示文档的最大宽度
  (corfu-popupinfo-max-width 80)
  ;; 显示文档的最大高度
  (corfu-popupinfo-max-height 20))

;; corfu-history: 补全历史记录（包含在 corfu 的 extensions 目录中）
(use-package corfu-history
  :ensure nil
  :after corfu
  :config
  (corfu-history-mode))

;; corfu-quick: 快速补全（包含在 corfu 的 extensions 目录中）
(use-package corfu-quick
  :ensure nil
  :after corfu
  :config
  ;; 使用数字键快速选择补全项
  (corfu-quick-mode))

;; cape: 补全后端增强
(use-package cape
  :vc (:url "https://github.com/minad/cape")
  :after corfu
  :init
  ;; 添加补全后端
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  :config
  ;; 移除默认的补全后端，避免重复
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-silent))

;; 美化配置
(with-eval-after-load 'corfu
  ;; 设置补全框的边框样式
  (set-face-attribute 'corfu-border nil
                      :background (face-attribute 'default :background))
  
  ;; 设置当前选中项的样式
  (set-face-attribute 'corfu-current nil
                      :background (face-attribute 'highlight :background)
                      :foreground (face-attribute 'default :foreground))
  
  ;; 设置补全项的样式
  (set-face-attribute 'corfu-default nil
                      :background (face-attribute 'default :background))
  
  ;; 设置补全框的阴影
  (customize-set-variable 'corfu-scroll-margin 4))

;; eldoc: 显示函数签名和文档
(use-package eldoc
  :ensure nil
  :custom
  ;; 显示文档的延迟时间（秒）
  (eldoc-idle-delay 0.3)
  ;; 最小字符数才显示文档
  (eldoc-minor-mode-string "")
  :config
  ;; 全局启用 eldoc 模式
  (global-eldoc-mode))

;; eldoc-box: 在弹窗中显示文档（更美观）
(use-package eldoc-box
  :vc (:url "https://github.com/casouri/eldoc-box" :branch "master")
  :after eldoc eglot
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  :custom
  ;; 弹窗位置
  (eldoc-box-position 'top-right)
  ;; 弹窗最大宽度
  (eldoc-box-max-pixel-width 600)
  ;; 弹窗最大高度
  (eldoc-box-max-pixel-height 400)
  ;; 弹窗边框宽度
  (eldoc-box-border 1)
  ;; 使用 markdown 渲染文档
  (eldoc-box-use-markdown t))

;; eglot 与 corfu 集成
(with-eval-after-load 'eglot
  ;; 使用 corfu 作为补全前端
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
        '((eglot (styles . (orderless)))))
  ;; 启用 eglot 的文档显示
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  ;; 配置文档显示
  (setq eglot-extend-to-xref t)
  ;; 启用签名帮助
  (setq eglot-signature-help-mode t))

;; 按键绑定
(with-eval-after-load 'corfu
  ;; 在 corfu 映射中添加一些有用的按键
  (define-key corfu-map (kbd "M-d") #'corfu-quick-complete)
  (define-key corfu-map (kbd "M-l") #'corfu-show-location)
  (define-key corfu-map (kbd "M-h") #'corfu-popupinfo-toggle))

;; eglot 文档查看按键绑定
(with-eval-after-load 'eglot
  ;; 查看符号的文档（hover）
  (define-key eglot-mode-map (kbd "C-c C-d") #'eldoc-box-help-at-point)
  ;; 显示签名帮助
  (define-key eglot-mode-map (kbd "C-c C-s") #'eglot-signature-help))

(provide 'init-eglot)
;;; init-eglot.el ends here

