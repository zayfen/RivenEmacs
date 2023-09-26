;;; modules/re-eglot-corfu.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 zayfen

;; Author zayfen (zhangyunfeng0101@gmail.com)

;;; Code

(use-package corfu
  :straight t
  :demand t
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

;; Add extensions
(use-package cape
  :straight t
  :demand t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-] p" . completion-at-point) ;; capf
         ("M-] t" . complete-tag)        ;; etags
         ("M-] d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-] h" . cape-history)
         ("M-] f" . cape-file)
         ("M-] k" . cape-keyword)
         ("M-] s" . cape-elisp-symbol)
         ("M-] e" . cape-elisp-block)
         ("M-] a" . cape-abbrev)
         ("M-] l" . cape-line)
         ("M-] w" . cape-dict)
         ("M-] :" . cape-emoji)
         ("M-] \\" . cape-tex)
         ("M-] _" . cape-tex)
         ("M-] ^" . cape-tex)
         ("M-] &" . cape-sgml)
         ("M-] r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))


(use-package yasnippet-snippets
  :ensure t
  :defer t
  :after yasnippet)


(use-package eglot
  :straight (:type built-in)
  :defer t
  :hook ((prog-mode) . eglot-ensure)
  :config
  (setq read-process-output-max (* 1024 1024))
  (push :documentHighlightProvider eglot-ignored-server-capabilities)

  (+map! :keymaps 'eglot-mode-map
    :infix "c"
    "a" '(eglot-code-actions :wk "Code actions")
    "e" '(eglot--diagnostics :wk "Diagnostics")
    "f" '(eglot-format-buffer :wk "Format code")
    "F" '(eglot-code-action-quickfix :wk "Quick fix")
    "i" '(eglot-find-implementation :wk "Find implementation")
    "o" '(eglot-code-action-organize-imports :wk "Organize imports")
    "r" '(eglot-rename :wk "Rename")
    "t" '(eglot-find-typeDefinition :wk "Find the typeDefinition")))


(provide 're-eglot-corfu)
;;; re-eglot-corfu.el ends here
