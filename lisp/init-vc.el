;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-vc.el --- Git and version control

(use-package magit
  :custom
  (magit-diff-refine-hunk t)
  (magit-revision-show-gravatars t)
  (magit-save-repository-buffers nil)
  ;; Show in new window
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)
  :hook (prog-mode . (lambda ()
                       (keymap-global-unset "C-c M-g"))))


(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode 1))


(use-package diff-hl
  :hook (find-file    . diff-hl-mode)
  :hook (dired-mode   . diff-hl-dired-mode)
  :hook (vc-dir-mode  . diff-hl-dir-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :custom
  (diff-hl-draw-borders nil)
  :config
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(use-package git-timemachine
  :custom
  (git-timemachine-show-minibuffer-details t))


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
