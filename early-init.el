;;; early-init.el --- RivenEmacs early initialization tweaks -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

(setq
 ;; Do not make installed packages available when Emacs starts
 package-enable-at-startup nil
 ;; HACK: Increase the garbage collection (GC) threshold for faster startup.
 ;; This will be overwritten when `gcmh-mode' (a.k.a. the Garbage Collector
 ;; Magic Hack) gets loaded in the `re-gc' module (see "init.el").
 gc-cons-threshold most-positive-fixnum
 garbage-collection-messages t
 ;; Do not wast time checking the modification time of each file
 load-prefer-newer noninteractive
 ;; Remove some unneeded UI elements
 default-frame-alist '((tool-bar-lines . 0)
                       (menu-bar-lines . 0)
                       (vertical-scroll-bars)
                       (mouse-color . "blue")
                       (left-fringe . 8)
                       (right-fringe . 13)
                       (fullscreen . maximized))
 ;; Explicitly set modes disabled in `default-frame-alist' to nil
 tool-bar-mode nil
 menu-bar-mode nil
 scroll-bar-mode nil
 )

;; NOTE: In Emacs29+, frames can have a transparent background via the
;; `alpha-background' parameter. For a better experience, this value should be
;; set early before any frame gets created (i.e. in "early-init.el"). RivenEmacs
;; uses the "$MINEMACS_ALPHA" environment variable that can be set to an integer
;; value in the [1-100] range (the alpha percentage). When this variable is not
;; set, Emacs will load the default GUI (without background alpha), and when it
;; is set but the value is not valid, RivenEmacs will fallback to the default
;; alpha of 93%.
(when (>= emacs-major-version 29)
  (when-let* ((alpha (getenv "MINEMACS_ALPHA"))
              (alpha (string-to-number alpha)))
    (push (cons 'alpha-background (if (or (zerop alpha) (> alpha 100)) 93 alpha))
          default-frame-alist)))

;; HACK: In `lsp-mode' (see the `re-lsp' module), the user can define the
;; "$LSP_USE_PLISTS=true" to improve `lsp-mode' performances. We set this
;; environment variable here so we don't need to add it to the system's
;; environment variables.
(setenv "LSP_USE_PLISTS" "true")

;; Load RivenEmacs variables from the `re-vars' core module.
(load (expand-file-name "core/re-vars.el" (file-name-directory (file-truename load-file-name))) nil t)

;; Load the user early configuration file from "$MINEMACSDIR/early-config.el"
;; if it exists.
(let ((early-config-path (concat rivenemacs-config-dir "early-config.el")))
  (when (file-exists-p early-config-path)
    (load early-config-path nil (not rivenemacs-verbose))))


(setq package-archives '(("gnu-elpa"   . "http://1.15.88.122/gnu/")
                         ("nongun-elpa" . "http://1.15.88.122/nongnu/")
                         ("melpa" . "http://1.15.88.122/melpa/")
                         ("melpa-stable" . "http://1.15.88.122/stable-melpa/")
                         ("org" . "http://1.15.88.122/org/")))
(package-initialize)
