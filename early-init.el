;;; early-init.el --- early initialization tweaks -*- lexical-binding: t; -*-

(setq
 package-enable-at-startup nil
 gc-cons-threshold most-positive-fixnum
 load-prefer-newer noninteractive
 default-frame-alist '((tool-bar-lines . 0)
                       (menu-bar-lines . 0)
                       (vertical-scroll-bars)
                       (mouse-color . "blue")
                       (left-fringe . 8)
                       (right-fringe . 8)
                       (undecorated-round . t) ; round corner
                       (fullscreen . maximized))
 tool-bar-mode nil
 menu-bar-mode nil
 scroll-bar-mode nil
 )

;; load env
(load (expand-file-name "lisp/env.el" (file-name-directory (file-truename (or load-file-name (buffer-file-name))))) nil t)


;; use <command> key as <meta>
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)


(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)
