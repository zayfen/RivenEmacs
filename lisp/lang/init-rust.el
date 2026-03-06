;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-rust.el --- Rust lang development config

(use-package rust-ts-mode
  :mode ("\\.rs\\'" . rust-ts-mode)
  :init
  (setq rust-ts-mode-treesitter-derive t)
  :config
  (setq rust-ts-mode-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode)))

;; (use-package rustic
;;   :ensure t
;;   :after (rust-ts-mode)
;;   :config
;;   (setq rustic-lsp-client nil)          ; available values are: lsp eglot
;;   (setq rustic-format-on-save nil)
;;   :custom
;;   (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
;;   (rustic-cargo-use-last-stored-arguments t))

(use-package cargo
  :hook (rust-ts-mode . cargo-minor-mode))

;; 使用内置的 flymake 支持 Rust
;; Rust 语言服务器 (rust-analyzer) 通过 lsp-bridge 提供语法检查

(provide 'init-rust)

;;; init-rust.el ends here
