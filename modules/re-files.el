;;; re-files.el --- File management -*- lexical-binding: t; -*-

;;; Commentary:
;;Copyright (C) 2022-2023  Abdelhak Bougouffa
;;Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


;; Code:

(use-package dirvish
  :straight t
  :hook (rivenemacs-after-startup . dirvish-override-dired-mode)
  :custom
  (dirvish-attributes '(subtree-state all-the-icons file-size vc-state git-msg))
  (dirvish-cache-dir (+directory-ensure rivenemacs-cache-dir "dirvish/"))
  (dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  (dirvish-side-width 30)
  (dirvish-fd-default-dir "~/")
  (dirvish-use-header-line t) ; 'global make header line span all panes
  (dirvish-use-mode-line t)
  :init
  (+map! :infix "d"
    "" '(nil :wk "Dirvish")
    "d" '(dirvish :wk "Open Dirvish")
    "q" '(dirvish-quick-access :wk "Dirvish quick access")
    "f" '(dirvish-fd :wk "Dirvish fd"))
  :config
  (+nvmap! :keymaps 'dirvish-mode-map
    "q" #'dirvish-quit
    "s" #'dirvish-subtree-toggle
    "y" #'dirvish-yank-menu))

(use-package vlf-setup
  :straight vlf
  :after rivenemacs-loaded
  :demand t)

(use-package treemacs
  :straight t
  :init
  (+map!
    "op" '(treemacs :wk "Side panel"))
  :custom
  (treemacs-persist-file (concat rivenemacs-local-dir "treemacs/persist.el"))
  (treemacs-last-error-persist-file (concat rivenemacs-local-dir "treemacs/last-error-persist.el"))
  (treemacs-width 30))

(use-package treemacs-all-the-icons
  :straight t
  :after treemacs all-the-icons
  :demand t
  :config
  (treemacs-load-theme "all-the-icons"))


(provide 're-files)
