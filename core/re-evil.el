;; re-evil.el --- Emacs as Vim! -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


(use-package evil
  :straight t
  :hook (rivenemacs-after-startup . evil-mode)
  :preface
  (setq evil-want-keybinding nil)
  :custom
  (evil-want-C-i-jump nil)
  (evil-want-fine-undo t)
  (evil-want-Y-yank-to-eol t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-kill-on-visual-paste nil)
  (evil-respect-visual-line-mode t)
  (evil-ex-interactive-search-highlight 'selected-window)
  :config
  (+map!
    ;; buffer
    "bN" '(evil-buffer-new :wk "New buffer")
    ;; window
    "ww" '(evil-window-next :wk "Next")
    "wW" '(evil-window-prev :wk "Previous")
    "ws" '(evil-window-split :wk "Split")
    "wv" '(evil-window-vsplit :wk "Vertical split")
    "wr" '(evil-window-rotate-downwards :wk "Rotate downwards")
    "wR" '(evil-window-rotate-upwards :wk "Rotate upwards")
    "w+" '(evil-window-increase-width :wk "Increase width")
    "w-" '(evil-window-decrease-width :wk "Decrease width"))

  ;; Use `evil-search' instead of `isearch'
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Ask for a buffer when splitting windows
  (with-eval-after-load 'consult
    (dolist (fn '(evil-window-split evil-window-vsplit))
      (advice-add
       fn :after
       (defun +evil--cunsult-buffer-after-window-split-a (&rest _)
         (consult-buffer))))))

(use-package evil-collection
  :straight t
  :after evil rivenemacs-loaded
  :demand t
  :config
  (evil-collection-init
   (seq-filter
    (lambda (mode)
      (not (memq mode '(evil-mc ; Default bindings for `evil-mc' are messy
                        mu4e ; TEMP: until `evil-collection-mu4e' gets fixed, see github.com/emacs-evil/evil-collection/issues/695
                        elisp-mode)))) ; I don't like "gz" for `ielm', I like "gr" though
    evil-collection-mode-list))

  ;; Use "gr" to find references for elisp mode
  (with-eval-after-load 'elisp-mode
    (when evil-collection-want-find-usages-bindings
      (evil-collection-define-key 'normal 'emacs-lisp-mode-map
        "gr" 'xref-find-references)))

  ;; TEMP: Fix `mu4e' evil integraion
  (with-eval-after-load 'mu4e
    (require 'loadhist) ; to use `feature-file'

    ;; To avoid calling `evil-collection-mu4e--main-action-str'
    (defvar evil-collection-mu4e-new-region-basic nil)

    (require 'evil-collection-mu4e
             (concat (file-name-directory (feature-file 'evil-collection))
                     "modes/mu4e/evil-collection-mu4e.el"))

    (evil-collection-mu4e-set-state)
    (evil-collection-mu4e-set-bindings)

    ;; Fix some missed up bindings
    (defalias 'mu4e~view-quit-buffer #'mu4e-view-quit)

    (add-hook 'org-mode-hook #'evil-collection-mu4e-org-set-header-to-normal-mode)
    (add-hook 'mu4e-compose-pre-hook #'evil-collection-mu4e-org-set-header-to-insert-mode)))

(use-package evil-snipe
  :straight t
  :hook (rivenemacs-after-startup . evil-snipe-mode)
  :hook (rivenemacs-after-startup . evil-snipe-override-mode)
  :custom
  (evil-snipe-scope 'buffer)
  (evil-snipe-smart-case t)
  (evil-snipe-auto-scroll t))

(use-package evil-numbers
  :straight t
  :init
  (+nmap!
    "g+" #'evil-numbers/inc-at-pt
    "g=" #'evil-numbers/inc-at-pt
    "g-" #'evil-numbers/dec-at-pt)
  (+vmap!
    "g+" #'evil-numbers/inc-at-pt-incremental
    "g=" #'evil-numbers/inc-at-pt-incremental
    "g-" #'evil-numbers/dec-at-pt-incremental))

(use-package evil-nerd-commenter
  :straight t
  :commands evilnc-comment-operator
  :init
  (+nvmap! "gc" #'evilnc-comment-operator))

(use-package evil-escape
  :straight t
  :hook (evil-mode . evil-escape-mode)
  :custom
  ;; The default "fd" interfere with the "f" (bound to `evil-snipe-f') binding.
  (evil-escape-key-sequence "kj")
  (evil-escape-unordered-key-sequence t)) ; "kj" or "jk"


(provide 're-evil)