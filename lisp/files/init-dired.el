;; -*- coding: utf-8; lexical-binding: t -*-


;;; Code:
(require 'cl-lib)
(require 'subr-x)

(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file))

(defun riven/dired-gnu-ls-program ()
  "Return a GNU ls executable for Dired, or nil when unavailable."
  (or (executable-find "gls")
      (cl-some (lambda (path)
                 (when (file-executable-p path)
                   path))
               '("/opt/homebrew/bin/gls"
                 "/usr/local/bin/gls"))))

(defun riven/dired-configure-listing-program ()
  "Configure Dired listing command for GNU ls or BSD ls fallback."
  (if-let* ((gnu-ls (riven/dired-gnu-ls-program)))
      (setq insert-directory-program gnu-ls
            dired-use-ls-dired t
            dired-listing-switches
            "-l --almost-all --human-readable --group-directories-first --no-group")
    (setq dired-use-ls-dired nil
          dired-listing-switches "-lah")))

;; Note: On macOS, you may need to install coreutils: brew install coreutils
(use-package dirvish
  :ensure t
  :init
  (riven/dired-configure-listing-program)
  :bind (("C-x d" . dirvish)
         :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
         ("?"   . dirvish-dispatch)
         ("a"   . dirvish-quick-access)
         ("f"   . dirvish-file-info-menu)
         ("y"   . dirvish-yank-menu)
         ("N"   . dirvish-narrow)
         ("~"   . dirvish-history-last)
         ("^"   . dired-up-directory)
         ("h"   . dirvish-history-jump) ; remapped `describe-mode'
         ("r"   . dirvish-renaming-menu)      ; rename file
         ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
         ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
         ("TAB" . dirvish-subtree-toggle)
         ("M-f" . dirvish-history-go-forward)
         ("M-b" . dirvish-history-go-backward)
         ("M-l" . dirvish-ls-switches-menu)
         ("M-m" . dirvish-mark-menu)
         ("M-t" . dirvish-layout-toggle)
         ("M-s" . dirvish-setup-menu)
         ("M-e" . dirvish-emerge-menu)
         ("M-j" . dirvish-fd-jump))
  :custom
  (dirvish-attributes '(subtree-state collapse file-size file-time vc-state git-msg vscode-icon))
  (dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  (dirvish-side-width 40)
  (dirvish-vscode-icon-size 20)
  (dirvish-fd-default-dir "~/")
  (dirvish-use-header-line t) ; 'global make header line span all panes
  (dirvish-use-mode-line t)
  :config
  (dirvish-override-dired-mode)
  (dirvish-peek-mode t) ; Preview files in minibuffer
  (dirvish-side-follow-mode t) ; similar to `treemacs-follow-mode'
  (setq delete-by-moving-to-trash t)
  (riven/dired-configure-listing-program)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-mouse-drag-files t)
  (setq mouse-drag-and-drop-region-cross-program t)
  ;;  (if os/mac (setq insert-directory-program "gls"))
)


(provide 'init-dired)
