;;; re-ui.el --- UI stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


(use-package svg-lib
  :straight t
  :custom
  (svg-lib-icons-dir (concat rivenemacs-cache-dir "svg-lib/icons/")))

(use-package visual-fill-column
  :straight t
  :custom
  (visual-fill-column-width nil)
  (visual-fill-column-center-text t))

(use-package mixed-pitch
  :straight t
  :init
  (+map! "tm" #'mixed-pitch-mode)
  :hook ((text-mode . mixed-pitch-mode)
         (org-mode . mixed-pitch-mode)
         (markdown-mode . mixed-pitch-mode))
  :custom
  (mixed-pitch-variable-pitch-cursor 'box)
  :config
  (setq mixed-pitch-fixed-pitch-faces
        (delete-dups
         (append mixed-pitch-fixed-pitch-faces
                 '(
                   org-date
                   org-footnote
                   org-drawer
                   org-special-keyword
                   org-property-value
                   org-column-title
                   org-column
                   org-cite
                   org-cite-key
                   org-ref-cite-face
                   org-tag
                   org-table
                   org-tag-group
                   org-formula
                   org-meta-line
                   org-document-info-keyword
                   org-block
                   org-block-begin-line
                   org-block-end-line
                   org-inline-src-block
                   org-src
                   org-verbatim
                   org-code
                   org-quote
                   org-verse
                   org-latex-and-related
                   org-macro
                   org-link
                   org-sexp-date
                   org-todo
                   org-done
                   font-lock-comment-face
                   font-lock-comment-delimiter-face))))
  )

(use-package re-writing-mode
  :init
  (+map! "tw" #'+writing-mode))

(use-package page-break-lines
  :straight t
  :hook ((prog-mode text-mode special-mode) . page-break-lines-mode))

(use-package focus
  :straight t
  :init
  (+map! "tF" #'focus-mode))


(use-package solaire-mode
  :straight t
  :config
  (solaire-global-mode +1))


(use-package transwin
  :straight t
  :custom
  (transwin-delta-alpha 5)
  (transwin-parameter-alpha 'alpha-background)
  :bind
  ("C-M-=" . transwin-inc)
  ("C-M--" . transwin-dec)
  ("C-M-0" . transwin-toggle))


(provide 're-ui)
