;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Should be configured in per-project basis, good documentation at:
;; github.com/cjohansson/emacs-ssh-deploy#deployment-configuration-examples

;; Code:

(use-package ssh-deploy
  :straight t
  :hook ((after-save . ssh-deploy-after-save)
         (find-file . ssh-deploy-find-file))
  :init
  (+map! "od" '(ssh-deploy-hydra/body :wk "ssh-deploy"))
  :config
  (ssh-deploy-hydra "C-c C-z"))

(use-package tldr
  :straight t
  :init
  (+map! "h?" #'tldr)
  :custom
  (tldr-enabled-categories '("common" "linux" "osx")))

(use-package vterm
  :straight t
  :init
  (+map!
    "ot" #'vterm)
  ;; Hide vterm install window
  (add-to-list
   'display-buffer-alist
   `(" \\*Install vterm\\*"
     (display-buffer-no-window)
     (allow-no-window . t)))
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 5000)
  (vterm-tramp-shells '(("docker" "/bin/bash")))
  :config
  (define-key vterm-mode-map [return] #'vterm-send-return))

(use-package multi-vterm
  :straight t
  :init
  (+map!
    "oT" #'multi-vterm-project)
  ;; Show at buttom
  (add-to-list
   'display-buffer-alist
   `("\\*vterminal - .*\\*" ;; multi-vterm-project / dedicated
     (display-buffer-reuse-window display-buffer-in-direction)
     (direction . bottom)
     (dedicated . t)
     (reusable-frames . visible)
     (window-height . 0.3)))
  :custom
  (multi-vterm-dedicated-window-height-percent 30))

(use-package docker
  :straight t
  :init
  (+map! "ok" #'docker))

(use-package docker-compose-mode
  :straight t)

;; Emacs 29 comes with `dockerfile-ts-mode'
(use-package dockerfile-ts-mode
  :straight (:type built-in)
  :mode "/Dockerfile\\'")
(use-package dockerfile-mode
  :straight t)

(use-package systemd
  :straight t)

(use-package pkgbuild-mode
  :straight t
  :config
  (+map-local! :keymaps 'pkgbuild-mode-map
    "b" #'pkgbuild-makepkg
    "a" #'pkgbuild-tar
    "r" #'pkgbuild-increase-release-tag
    "u" #'pkgbuild-browse-url
    "m" #'pkgbuild-update-sums-line
    "s" #'pkgbuild-update-srcinfo
    "e" #'pkgbuild-etags))

(use-package journalctl-mode
  :straight t
  :config
  (+map-local! :keymaps 'journalctl-mode-map
    "J" #'journalctl-next-chunk
    "K" #'journalctl-previous-chunk))

(use-package logview
  :straight t
  :init
  (+map! :infix "t"
    "l" '(log-view-mode :wk "+logview-mode")))

(use-package quickrun
  :ensure t
  :commands (quickrun)
  :bind ("<f5>" . quickrun)
  :init
  (+map! :infix "o"
    "q" '(quickrun :wk "Quick run")))

(use-package devdocs
  :ensure t
  :commands (devdoc-lookup)
  :config
  (add-hook 'python-mode-hook
          (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  (add-hook 'typescript-mode-hook
          (lambda () (setq-local devdocs-current-docs '("javascript"))))
  (add-hook 'c-mode-hook
            (lambda () (setq-local devdocs-current-docs '("c"))))
  (add-hook 'c++-mode-hook
            (lambda () (setq-local devdocs-current-docs '("c"))))
  (add-hook 'rust-mode-hook
            (lambda () (setq-local devdocs-current-docs '("rust"))))
  )


(provide 're-tools)
