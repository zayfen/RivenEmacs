;;; re-multi-cursors.el --- Programming stuff -*- lexical-binding: t; -*-



(use-package iedit
  :straight t
  :after rivenemacs-lazy
  :demand t
  :preface
  (+fn-inhibit-messages! iedit-update-key-bindings)
  :config
  ;; Define a new face for iedit occurrence highlighting
  (defface iedit-occurrence
    '((t (:background "#1A8899" :foreground "black")))
    "Face for iedit occurrence highlighting.")
  (add-hook 'iedit-mode-hook
            (lambda ()
              (set-face-attribute 'iedit-occurrence nil :background "#1A8899" :foreground "black"))))


(use-package multiple-cursors
  :straight t
  :after rivenemacs-lazy
  :demand t
  )


(provide 're-multi-cursors)
