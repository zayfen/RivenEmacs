(setq org-directory "~/Qsync/Org/")
(setq org-agenda-files (list "~/Qsync/Org/GTD/todo.org" "~/Qsync/Org/GTD/done.org"))

;; Improve org mode looks
(setq-default org-startup-indented t
              org-pretty-entities t
              org-use-sub-superscripts "{}"
              org-hide-emphasis-markers t
              org-startup-with-inline-images t
              org-image-actual-width '(300))

(use-package org
  :straight (:type built-in)
  :after rivenemacs-loaded)

(use-package org-appear
  :ensure t
  :hook
  (org-mode . org-appear-mode))

(use-package org-modern
  :ensure t
  :hook
  (org-mode . global-org-modern-mode)
  :custom
  (org-modern-keyword nil)
  (org-modern-checkbox nil)
  (org-modern-table nil))

;; LaTeX previews
(use-package org-fragtog
  :ensure t
  :custom
  (org-startup-with-latex-preview t)
  :hook
  (org-mode . org-fragtog-mode)
  )

;; Distraction-free writing
(defun ews-distraction-free ()
  "Distraction-free writing environment using Olivetti package."
  (interactive)
  (if (equal olivetti-mode nil)
      (progn
        (window-configuration-to-register 1)
        (delete-other-windows)
        (text-scale-set 2)
        (olivetti-mode t))
    (progn
      (if (eq (length (window-list)) 1)
          (jump-to-register 1))
      (olivetti-mode 0)
      (text-scale-set 0))))

(use-package olivetti
  :ensure t
  :demand t
  :bind
  (("<f9>" . ews-distraction-free)))


(use-package org-roam
  :straight t
  :init
  (+map! :infix "n"
    "f" #'org-roam-node-find
    "r" #'org-roam-ref-find
    "i" #'org-roam-node-insert
    "R" #'org-roam-node-random))

(use-package org-roam-ui
  :straight t
  :init
  (+map! "nR" #'org-roam-ui-open))

(use-package consult-org-roam
  :straight t
  :after org-roam
  :demand t
  :init
  (+map! :infix "n"
    "s" #'consult-org-roam-search
    "l" #'consult-org-roam-forward-links
    "b" #'consult-org-roam-backlinks
    "F" #'consult-org-roam-file-find)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r) ; custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-after-buffers t)
  :config
  (consult-org-roam-mode 1)
  ;; Eventually suppress previewing for certain functions
  (consult-customize consult-org-roam-forward-links :preview-key (kbd "M-.")))

(use-package ox-gfm
  :defer t
  :ensure t)

(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode))

(use-package grip-mode
  :ensure t
  :config
  (setq grip-preview-use-webkit nil))

(use-package valign
  :ensure t
  :hook ((markdown-mode  org-mode) . valign-mode))

(use-package org-alert
  :ensure t
  :custom
  ((org-alert-notification-title "Org Agenda"))
  :init
  (require 'org-alert)
  (org-alert-enable))

(use-package org-super-agenda
  :ensure t
  :straight t
  )


(provide 're-org-pro)

;; re-org-pro.el ends here
