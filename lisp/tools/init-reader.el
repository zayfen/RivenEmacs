;; -*- coding: utf-8; lexical-binding: t -*-

;;; Code:
;; books reader in emacs

;; `reader' needs the `render-core' dynamic module.  Guard the whole
;; use-package form so a missing/unbuilt module does not make startup noisy.
(if (locate-library "render-core")
    (use-package reader
      :vc (:url "https://codeberg.org/divyaranjan/emacs-reader" :make "all"))
  (message "[reader] render-core 动态模块未构建，跳过 reader 配置"))

(provide 'init-reader)
;; init-reader.el ends here.
