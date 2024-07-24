(setq user-emacs-directory local-dir)

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

;; add extra load path
;; only add lisp/ dir to load path
(setq load-path (append (list lisp-dir) load-path))



(setq
 ;; Set `use-package' to verbose when RivenEmacs is started in verbose mode
 use-package-verbose 1
 ;; Defer loading packages by default, use `:demand' to force loading a package
 use-package-always-defer t)

;; load env from exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;; use-package with package.el:
(use-package dashboard
  :vc (:fetcher github :repo emacs-dashboard/emacs-dashboard)
  :config
  (dashboard-setup-startup-hook))

;; (require 'better-defaults)
(require 'init-use-package)
(require 'init-theme)
(require 'init-font)
(require 'init-undo)

;; init thirdparty packages
(require 'init-which-key)
(require 'init-general)


(require 'init-consult)
(require 'init-vertico)
(require 'init-crux)
(require 'init-embark)
(require 'init-editor)
(require 'init-format)
;; important: tree-sitter
(require 'init-treesit)

(require 'init-lsp-bridge)

;; init git
(require 'init-vc)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(git-modes diff-hl forge magit-todos magit undo-fu-session undo-fu vundo link-hint lsp-bridge-jdtls lsp-bridge yasnippet-snippets yasnippet dashboard orderless vertico vc-use-package general which-key))
 '(package-vc-selected-packages
   '((lsp-bridge :vc-backend Git :url "https://github.com/manateelazycat/lsp-bridge")
     (dashboard :vc-backend Git :url "https://github.com/emacs-dashboard/emacs-dashboard")
     (vertico :vc-backend Git :url "https://github.com/minad/vertico")
     (vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
