;;; re-jump.el --- Smart jump -*- lexical-binding: t; -*-

(use-package better-jumper
  :straight t
  :hook (rivenemacs-after-startup . better-jumper-mode)
  :config
  ;; Map extra mouse buttons to jump forward/backward
  (global-set-key [mouse-8] #'better-jumper-jump-backward)
  (global-set-key [mouse-9] #'better-jumper-jump-forward))

(use-package dumb-jump
  :straight t
  :commands
  +dumb-jump-hydra/body
  :after xref
  :custom
  (dumb-jump-selector 'completing-read)
  :init
  ;; remove etag feature and only use dumb-jump as xref-backend
  (setq xref-backend-functions (remq 'etags--xref-backend xref-backend-functions))
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)

  (+map!
    "j" '(+dumb-jump-hydra/body :wk "+dumb-jump-hydra"))

  :config
  ;; Define Hydra keybinding (from the repo's examples)
  (defhydra +dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump."
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")))

(use-package smart-jump
  :ensure t
  :commands (smart-jump-go smart-jump-back smart-jump-references)
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back)
         ("M-?" . smart-jump-references))
  :config
  (smart-jump-setup-default-registers)
  (smart-jump-register :modes '(prog-mode)
                       :jump-fn '#lsp-bridge-find-def
                       :pop-fn '#lsp-bridge-find-def-return
                       :refs-fn '#lsp-bridge-find-references
                       :should-jump t
                       :heuristic 'point
                       :async 500
                       :order 1))

(use-package goto-last-point
  :ensure t
  :init
  (global-set-key (kbd "C-<") 'goto-last-point)
  :commands goto-last-point
  :config
  (goto-last-point-mode))

(provide 're-jump)
