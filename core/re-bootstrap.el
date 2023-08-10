;; re-bootstrap.el --- Bootstrap packages (straight & use-package) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

(setq
 ;; Base directory
 straight-base-dir rivenemacs-local-dir
 ;; Add Emacs version and the Git hash to the build directory to avoid problems
 straight-build-dir (format "build-%s%s" emacs-version
                            (if emacs-repository-version
                                (format "-%s" (substring emacs-repository-version 0 8))
                              ""))
 ;; TEMP: Use the "master" branch on straight.el's repo (switch back to
 ;; "develop" when issue #28 gets fixed in upstream)
 straight-repository-branch "master"
 ;; Do not clone all project history, just the last worktree (--depth 1)
 straight-vc-git-default-clone-depth '(1 single-branch)
 ;; Do not slow startup by checking for package modifs, check only on demand
 straight-check-for-modifications '(check-on-save find-when-checking))

;; TEMP: The "master" branch of straight.el uses the old variable names, so we
;; make sure to provide them otherwise it will fail to load.
(when (and (not (boundp 'native-comp-deferred-compilation))
           (boundp 'native-comp-jit-compilation))
  (defvaralias 'native-comp-deferred-compilation 'native-comp-jit-compilation)
  (defvaralias 'native-comp-deferred-compilation-deny-list 'native-comp-jit-compilation-deny-list))

;; prevent some packges from native-compile
(setq native-comp-deferred-compilation-deny-list '("blink-search" "lsp-bridge" "color-rg"))

;; Bootstraping straight.el
;; See: github.com/radian-software/straight.el#bootstrapping-straightel
(defvar bootstrap-version)
(let ((bootstrap-file (concat straight-base-dir "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure `use-package'
(unless (require 'use-package nil t)
  (straight-use-package 'use-package))

(setq
 ;; Set `use-package' to verbose when RivenEmacs is started in verbose mode
 use-package-verbose rivenemacs-verbose
 ;; Defer loading packages by default, use `:demand' to force loading a package
 use-package-always-defer t)

;; load env from exec-path-from-shell
(use-package exec-path-from-shell
  :straight t
  :ensure t
  :hook (rivenemacs-after-startup . exec-path-from-shell-initialize))

(use-package explain-pause-mode
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode"))



(provide 're-bootstrap)
