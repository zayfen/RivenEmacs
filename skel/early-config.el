;;; early-config.el -*- coding: utf-8-unix; lexical-binding: t; -*-

;; This file will be loaded at the end of `early-init.el', it can be used to set
;; some early initialization stuff, or to set some RivenEmacs variables, specially
;; these used in macros.

;; Set log level to `info' rather than `error'
(unless rivenemacs-verbose
  (setq rivenemacs-msg-level 2))

;; Enable full screen at startup
;; (if-let ((fullscreen (assq 'fullscreen default-frame-alist)))
;;     (setcdr fullscreen 'fullboth)
;;   (push '(fullscreen . fullboth) default-frame-alist))

;; Force loading lazy packages immediately, not in idle time
;; (setq rivenemacs-not-lazy t)

;; Setup a `debug-on-message' to catch a wired message!
;; (setq debug-on-message "Package cl is deprecated")
