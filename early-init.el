;;; early-init.el --- early initialization tweaks -*- lexical-binding: t; -*-

(setq
 package-enable-at-startup t            ;; set to true, so need not to call package-initialize any more.
 use-package-verbose 1
 ;; Defer loading packages by default, use `:demand' to force loading a package
 use-package-always-defer t
 use-package-always-ensure t
 gc-cons-threshold most-positive-fixnum
 load-prefer-newer noninteractive
 default-frame-alist '((tool-bar-lines . 0)
                       (menu-bar-lines . 0)
                       (vertical-scroll-bars)
                       (mouse-color . "blue")
                       (left-fringe . 4)
                       (right-fringe . 4)
                       (undecorated-round . t) ; round corner
                       (fullscreen . maximized))
 tool-bar-mode nil
 menu-bar-mode nil
 scroll-bar-mode nil
 create-lockfiles nil
 warning-minimum-level :error
 read-process-output-max (* 1024 1024)
 inhibit-compacting-font-caches t
 jit-lock-defer-time 0.1
 inhibit-automatic-native-compilation t
 )

(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))
(advice-add 'tool-bar-setup :override #'ignore)

(if (boundp 'warning-suppress-log-types)
    (add-to-list 'warning-suppress-log-types '(unlock-file)))
(if (boundp 'warning-suppress-types)
    (add-to-list 'warning-suppress-types '(unlock-file)))

;; load env
(load (expand-file-name "lisp/env.el" (file-name-directory (file-truename (or load-file-name (buffer-file-name))))) nil t)

(setq user-emacs-directory local-dir)
(setq load-path (append (list lisp-dir) load-path))
(setq load-path (append (list repo-dir) load-path))


(setq custom-file (concat root-dir "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;; use <command> key as <meta>
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)


(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

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

;;dont need (package-initialize) on emacs27+
(when (< emacs-major-version 27)
  (package-initialize))
