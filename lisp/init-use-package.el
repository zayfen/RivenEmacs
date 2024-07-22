(use-package emacs
  :init
  (setq use-package-always-ensure t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package")))

(provide 'init-use-package)
