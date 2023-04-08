;;; completion.el --- Completion packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

(use-package consult
  :straight t
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :custom
  ;; Use `consult-xref' for `xref-find-references'
  (xref-show-xrefs-function #'consult-xref)
  ;; Better formatting for `view-register'
  (register-preview-function #'consult-register-format)
  :init
  (define-key minibuffer-local-map (kbd "C-r") #'consult-history)
  (define-key minibuffer-local-map (kbd "S-C-v") #'consult-yank-pop)
  (global-set-key (kbd "C-s") #'consult-line)
  (+map!
    ;; buffer
    "bl"  #'consult-line
    "bb"  #'consult-buffer
    "bB"  #'consult-buffer-other-window
    "bF"  #'consult-buffer-other-frame
    "bmM" #'consult-bookmark
    "bi"  #'consult-imenu
    "bO"  #'consult-outline
    ;; file
    "fr"  #'consult-recent-file
    ;; git/vc
    "gG"  #'consult-git-grep
    ;; search
    "s."  #'consult-ripgrep
    "sf"  #'consult-find
    "sM"  #'consult-man
    "st"  #'consult-locate
    "sh"  #'consult-history
    "sa"  #'consult-org-agenda
    ;; project
    "pl"  #'consult-line-multi
    "pi"  #'consult-imenu-multi
    ;; code
    "cm"  #'consult-flymake
    "cE"  #'consult-compile-error
    ;; unclassified
    "xc"  #'consult-complex-command
    ;; insert
    "iy"  #'consult-yank-from-kill-ring
    "ir"  '(nil :wk "register")
    "irr" #'consult-register
    "irl" #'consult-register-load
    "irs" #'consult-register-store
    ;; help
    "hu"  #'consult-theme
    "hI"  #'consult-info)
  (+map-local! :keymaps 'org-mode-map
    "h"   #'consult-org-heading)
  :config
  (setq-default completion-in-region-function #'consult-completion-in-region)

  ;; TWEAK: Fill the `initial' query of `consult' commands from
  ;; `thing-at-point'.
  ;; NOTE: Some `consult' commands have slightly different signature, the
  ;; `initial' argument can come first in some cases (like `consult-line') or
  ;; second in some other cases (like `condult-grep'). These two advices are
  ;; added to such family of commands so it is filled in the right place.
  (dolist (cmd '(consult-line ; `initial' comes first in these commands
                 consult-man))
    (advice-add
     cmd :around
     (defun +consult--dwim-first-arg-a (orig-fn &optional initial opt)
       (apply orig-fn
              (append
               (if (and (called-interactively-p) (not (minibufferp)))
                   (list (or initial (+region-or-thing-at-point)))
                 (list initial))
               (when opt (list opt)))))))

  (dolist (cmd '(consult-ripgrep ; `initial' comes second in these commands
                 consult-line-multi
                 consult-grep
                 consult-find))
    (advice-add
     cmd :around
     (defun +consult--dwim-second-arg-a (orig-fn &optional dir initial)
       (apply orig-fn
              (append
               (list dir)
               (if (and (called-interactively-p) (not (minibufferp)))
                   (list (or initial (+region-or-thing-at-point)))
                 (list initial))))))))

(use-package embark
  :straight t
  :init
  (global-set-key [remap describe-bindings] #'embark-bindings)
  (setq prefix-help-command #'embark-prefix-help-command)
  (+map! "." #'embark-act))

(use-package embark-consult
  :straight t
  :after embark consult
  :demand t)

(use-package marginalia
  :straight t
  :hook (rivenemacs-after-startup . marginalia-mode))

(use-package all-the-icons-completion
  :straight t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package orderless
  :straight t
  :after rivenemacs-loaded
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :straight t
  :hook (rivenemacs-after-startup . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 12)
  :init
  (add-to-list
   'load-path (concat
               straight-base-dir
               (format "straight/%s/vertico/extensions" straight-build-dir)))
  ;; In the minibuffer, "C-k" is be mapped to act like "<up>". However, in
  ;; Emacs, "C-k" have a special meaning of `kill-line'. So lets map "C-S-k"
  ;; to serve the original "C-k".
  (define-key minibuffer-local-map (kbd "C-S-k") #'kill-line)
  :config
  (with-eval-after-load 'evil
    (define-key vertico-map (kbd "C-j") #'vertico-next)
    (define-key vertico-map (kbd "C-k") #'vertico-previous)))

(use-package vertico-directory
  :after vertico
  :demand t
  :config
  (define-key vertico-map "\r" #'vertico-directory-enter)
  (define-key vertico-map "\d" #'vertico-directory-delete-char)
  (define-key vertico-map "\M-\d" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (with-eval-after-load 'evil
    (define-key vertico-map (kbd "M-h") #'vertico-directory-up)))

(use-package vertico-repeat
  :hook (minibuffer-setup . vertico-repeat-save)
  :init
  (keymap-global-set "M-R" #'vertico-repeat))


(use-package blink-search
  :straight (:host github :repo "manateelazycat/blink-search" :files ("*" (:exclude ".git")))
  :commands (blink-search)
  :init
  (setq blink-search-browser-function
        (if (display-graphic-p)
            #'xwidget-webkit-browse-url
          #'eww))
  (+map! :infix "s"
    "s" '(blink-search :wk "Blink Search")))


(use-package color-rg
  :straight (:host github :repo "manateelazycat/color-rg" :files ("*" (:exclude ".git")))
  :commands (color-rg-search-input-in-project color-rg-search-symbol-in-project color-rg-search-input-in-current-file color-rg-search-symbol-in-current-file)
  :init
  (+map! :infix "s"
    "p" '(color-rg-search-symbol-in-project :wk "Search project at point")
    "P" '(color-rg-search-input-in-project :wk "Search project")
    "b" '(color-rg-search-symbol-in-current-file :wk "Search buffer at point")
    "B" '(color-rg-search-input-in-current-file :wk "Search buffer")))





(provide 're-completion)
