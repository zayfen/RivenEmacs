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
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-x p p" . completion-at-point) ;; capf
         ("C-x p t" . complete-tag)        ;; etags
         ("C-x p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-x p h" . cape-history)
         ("C-x p f" . cape-file)
         ("C-x p k" . cape-keyword)
         ("C-x p s" . cape-elisp-symbol)
         ("C-x p e" . cape-elisp-block)
         ("C-x p a" . cape-abbrev)
         ("C-x p l" . cape-line)
         ("C-x p w" . cape-dict)
         ("C-x p :" . cape-emoji)
         ("C-x p \\" . cape-tex)
         ("C-x p _" . cape-tex)
         ("C-x p ^" . cape-tex)
         ("C-x p &" . cape-sgml)
         ("C-x p r" . cape-rfc1345))
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
  (push :documentHighlightProvider eglot-ignored-server-capabilities))


(provide 're-eglot-corfu)
;;; re-eglot-corfu.el ends here
