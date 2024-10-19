;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-rust.el --- Rust lang development config

(use-package rust-ts-mode
  :ensure t
  :mode ("\\.rs'" . rust-ts-mode)
  :init
  (setq rust-mode-treesitter-derive t))

;; (use-package rustic
;;   :ensure t
;;   :after (rust-ts-mode)
;;   :config
;;   (setq rustic-lsp-client nil)          ; avaiable values are: lsp eglot
;;   (setq rustic-format-on-save nil)
;;   :custom
;;   (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")
;;   (rustic-cargo-use-last-stored-arguments t))


(provide 'init-rust)

;;; init-rust.el ends here
