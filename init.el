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

(require 'init-use-package)

(require 'init-theme)

(require 'init-which-key)
(require 'init-general)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(general which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
