;;; re-multi-cursors.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

(use-package iedit
  :straight t
  :after rivenemacs-lazy
  :demand t
  :preface
  (+fn-inhibit-messages! iedit-update-key-bindings))


(use-package multiple-cursors
  :straight t
  :after rivenemacs-lazy
  :demand t
  )


(provide 're-multi-cursors)
