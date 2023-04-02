;; rivenemacs-loaded.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;; Virtual module loaded at end of init.el (after custom-vars.el)
;; Used to synchronize loading some other stuff after loading Emacs

;; Run hooks
(when rivenemacs-after-startup-hook
  (setq rivenemacs-after-startup-hook (reverse rivenemacs-after-startup-hook))
  (+log! "Running %d `rivenemacs-after-startup-hook' hooks."
         (length rivenemacs-after-startup-hook))
  (run-hooks 'rivenemacs-after-startup-hook))

(if rivenemacs-not-lazy
    (require 'rivenemacs-lazy)
  (+eval-when-idle-for! 2
    (require 'rivenemacs-lazy)))

(+log! "Providing `rivenemacs-loaded'.")

(provide 'rivenemacs-loaded)
