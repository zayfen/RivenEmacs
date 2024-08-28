;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-vc.el --- Git and version control


(use-package magit
  :init
  (leader-def :infix "g"
    "g" #'magit-status
    "C" #'magit-clone
    "b" #'magit-blame
    "l" #'magit-log
    "d" #'magit-diff-dwim
    "s" #'magit-stage
    "i" #'magit-init)
  :custom
  (magit-diff-refine-hunk t)
  (magit-revision-show-gravatars t)
  (magit-save-repository-buffers nil)
  ;; Show in new window
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1))

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode 1))

(use-package forge
  :after magit
  :preface
  (setq forge-add-default-bindings nil)
  :init
  (leader-def :infix "g"
    "f" '(nil :wk "forge")
    "ff" #'forge-dispatch
    "fc" #'forge-create-post
    "fe" #'forge-edit-post
    "ft" #'forge-edit-topic-title
    "fs" #'forge-edit-topic-state
    "fd" #'forge-edit-topic-draft)
  :custom
  (forge-database-connector 'sqlite-builtin)
  (forge-database-file (concat local-dir "forge/database.sqlite")))


(use-package diff-hl
  :hook (find-file    . diff-hl-mode)
  :hook (dired-mode   . diff-hl-dired-mode)
  :hook (vc-dir-mode  . diff-hl-dir-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :init
  (leader-def "gs" #'diff-hl-stage-current-hunk)
  :custom
  (diff-hl-draw-borders nil)
  :config
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(use-package git-timemachine
  :init
  (leader-def "gt" #'git-timemachine-toggle)
  :custom
  (git-timemachine-show-minibuffer-details t))

;; Enforce git commit conventions.
;; See: chris.beams.io/posts/git-commit/
(use-package git-commit
  :after magit
  :custom
  (git-commit-summary-max-length 50)
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  :config
  (add-hook
   'git-commit-mode-hook
   (defun +git-gommit--set-fill-column-h ()
     (setq-local fill-column 72)))
  (add-hook
   'git-commit-setup-hook
   ;; Enter evil-insert-state for new commits
   (defun +git-commit--enter-evil-insert-state-maybe-h ()
     (when (and (bound-and-true-p evil-mode)
                (not (evil-emacs-state-p))
                (bobp)
                (eolp))
       (evil-insert-state))))
  (global-git-commit-mode 1))

(use-package git-modes
  :init
  :mode ("/.dockerignore\\'" . gitignore-mode))

(use-package ediff
  :custom
  ;; Split horizontally
  (ediff-split-window-function #'split-window-horizontally)
  ;; Setup all windows in one frame
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  :config
  (defvar +ediff--saved-window-config nil)

  ;; Save the current window configuration
  (add-hook
   'ediff-before-setup-hook
   (defun +ediff--save-window-config-h ()
     (setq +ediff--saved-window-config (current-window-configuration))))

  ;; Restore the saved window configuration on quit or suspend
  (dolist (hook '(ediff-quit-hook ediff-suspend-hook))
    (add-hook
     hook
     (defun +ediff--restore-window-config-h ()
       (when (window-configuration-p +ediff--saved-window-config)
         (set-window-configuration +ediff--saved-window-config)))
     101)))


(use-package repo
  :preface
  (defconst +repo-available-p (executable-find "repo"))
  :when +repo-available-p
  :init
  (leader-def "gr" #'repo-status))


;; config smerge-mode
(use-package smerge-mode
  :ensure nil
  :init
;;rr  (setq smerge-command-prefix "")

  :config
  ;; https://github.com/alphapapa/unpackaged.el#smerge-mode
  (defhydra hydra/smerge
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("q" nil "cancel" :color blue))

  :bind (:map smerge-mode-map
              ("C-c m" . hydra/smerge/body))
  )

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'init-vc)
