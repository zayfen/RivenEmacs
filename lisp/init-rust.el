;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-rust.el --- Rust lang development config

(use-package rust-ts-mode
  :ensure t
  :mode ("\\.rs'" . rust-ts-mode)
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (setq rust-ts-mode-indent-offset 2))

;; (use-package rustic
;;   :ensure t
;;   :after (rust-ts-mode)
;;   :config
;;   (setq rustic-lsp-client nil)          ; avaiable values are: lsp eglot
;;   (setq rustic-format-on-save nil)
;;   :custom
;;   (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")
;;   (rustic-cargo-use-last-stored-arguments t))

(use-package cargo
  :hook (rust-ts-mode . cargo-minor-mode))

(use-package flycheck-rust
  :after (flycheck-mode)
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'init-rust)

;;; init-rust.el ends here
