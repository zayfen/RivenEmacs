;; -*- coding: utf-8; lexical-binding: t -*-

(setq user-emacs-directory local-dir)
;; add extra load path
;; only add lisp/ dir to load path
(setq load-path (append (list lisp-dir) load-path))


(when (featurep 'native-compile)
  (setq
   native-comp-async-report-warnings-errors 'silent
   native-comp-verbose 1 		; can be 0 if config no error
   native-comp-debug 1 			; can be 0 if config no error
   ;; Make native compilation happens asynchronously.
   native-comp-jit-compilation t)

  ;; Set the right directory to store the native compilation cache to avoid
  ;; messing with "~/.emacs.d/".
  (startup-redirect-eln-cache (concat local-dir "eln/")))


;; load env from exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))


(require 'init-default)
(require 'init-use-package)
(require 'init-theme)
(require 'init-font)
(require 'init-undo)
(require 'init-autosave)

;; init thirdparty packages
(require 'init-which-key)
(require 'init-general)


(require 'init-consult)
(require 'init-vertico)
(require 'init-crux)
(require 'init-embark)
(require 'init-editor)
(require 'init-format)
(require 'init-jump)
(require 'init-editorconfig)

;; important: tree-sitter
(require 'init-treesit)
(require 'init-lsp-bridge)

;; init git
(require 'init-vc)

;; Languages ;TODO




;; keybindings
(require 'init-keybindings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(format-all yasnippet-snippets which-key vundo visual-regexp vertico vc-use-package undo-fu-session undo-fu super-save sudo-edit repo rainbow-delimiters prescient orderless mood-line marginalia magit-todos lsp-bridge link-hint iedit git-timemachine git-modes general forge expand-region exec-path-from-shell embark-consult diff-hl dashboard crux centered-window aggressive-indent-mode ace-window))
 '(package-vc-selected-packages
   '((aggressive-indent-mode :vc-backend Git :url "https://github.com/Malabarba/aggressive-indent-mode"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
