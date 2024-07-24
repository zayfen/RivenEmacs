;; -*- coding: utf-8; lexical-binding: t -*-

(setq
 ;; Set `use-package' to verbose when RivenEmacs is started in verbose mode
 use-package-verbose 1
 ;; Defer loading packages by default, use `:demand' to force loading a package
 use-package-always-defer t)

(use-package emacs
  :init
  (setq use-package-always-ensure t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package")))

(provide 'init-use-package)
