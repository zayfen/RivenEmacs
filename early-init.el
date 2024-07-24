;;; early-init.el --- RivenEmacs early initialization tweaks -*- lexical-binding: t; -*-


(setq
 ;; Do not make installed packages available when Emacs starts
 package-enable-at-startup nil
 ;; HACK: Increase the garbage collection (GC) threshold for faster startup.
 ;; This will be overwritten when `gcmh-mode' (a.k.a. the Garbage Collector
 ;; Magic Hack) gets loaded in the `re-gc' module (see "init.el").
 gc-cons-threshold most-positive-fixnum
 ;; Do not wast time checking the modification time of each file
 load-prefer-newer noninteractive
 ;; Remove some unneeded UI elements
 default-frame-alist '((tool-bar-lines . 0)
                       (menu-bar-lines . 0)
                       (vertical-scroll-bars)
                       (mouse-color . "blue")
                       (left-fringe . 8)
                       (right-fringe . 8)
                       (undecorated . t) ; round corner
                       (fullscreen . maximized))
 ;; Explicitly set modes disabled in `default-frame-alist' to nil
 tool-bar-mode nil
 menu-bar-mode nil
 scroll-bar-mode nil
 )

(setq native-comp-jit-compilation nil)
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; Load RivenEmacs variables from the `re-vars' core module.
(load (expand-file-name "core/re-vars.el" (file-name-directory (file-truename load-file-name))) nil t)

;; Load the user early configuration file from "$RIVENEMACSDIR/early-config.el"
;; if it exists.
(let ((early-config-path (concat rivenemacs-config-dir "early-config.el")))
  (when (file-exists-p early-config-path)
    (load early-config-path nil (not rivenemacs-verbose))))


;; (setq package-archives '(("gnu-elpa"   . "http://1.15.88.122/gnu/")
;;                          ("nongun-elpa" . "http://1.15.88.122/nongnu/")
;;                          ("melpa" . "http://1.15.88.122/melpa/")
;;                          ("melpa-stable" . "http://1.15.88.122/stable-melpa/")
;;                          ("org" . "http://1.15.88.122/org/")))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)
