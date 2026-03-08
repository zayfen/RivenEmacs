;;; -*- coding: utf-8; lexical-binding: t -*-

(eval-and-compile
  (let ((current-file (or load-file-name byte-compile-current-file buffer-file-name)))
    (when current-file
      (add-to-list 'load-path (file-name-directory current-file)))))

(require 'init-config)

(defun smarter-yas-expand-next-field ()
  "Try `yas-expand`, then move to next field if no expansion happened."
  (interactive)
  (let ((old-point (point))
        (old-tick (buffer-chars-modified-tick)))
    (yas-expand)
    (when (and (eq old-point (point))
               (eq old-tick (buffer-chars-modified-tick)))
      (ignore-errors (yas-next-field)))))

(use-package yasnippet
  :ensure t
  :custom
  (yas-snippet-dir rivenEmacs-snippets-dir)
  :hook ((prog-mode LaTeX-mode org-mode markdown-mode) . yas-minor-mode-on)
  :bind
  (:map yas-keymap
        ("TAB" . smarter-yas-expand-next-field)
        ([tab] . smarter-yas-expand-next-field)))

(use-package yasnippet-snippets
  :ensure t)

(use-package eglot
  :ensure nil
  :init
  (dolist (mode rivenEmacs-lsp-modes)
    (add-hook (intern (format "%s-hook" mode)) #'eglot-ensure))
  :bind (:map eglot-mode-map
              ("M-?" . xref-find-references))
  :custom
  ;; Built-in ElDoc behavior is enough; keep it compact and predictable.
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-idle-delay 0.15)
  (eglot-sync-connect nil)
  (eglot-autoshutdown t)
  (eglot-report-progress nil)
  (eglot-send-changes-idle-time 0.2)
  (eglot-extend-to-xref t)
  ;; Avoid cursor-line height jitter from code-action overlays in margin/nearby.
  ;; Keep hints in mode-line only to preserve stable line metrics in all languages.
  (eglot-code-action-indications '(mode-line))
  ;; Use ASCII indicator to avoid emoji fallback fonts affecting glyph height.
  (eglot-code-action-indicator "*")
  (eglot-events-buffer-config '(:size 0 :format short))
  :config
  (keymap-global-set "M-." #'xref-find-definitions)
  (keymap-global-set "M-," #'xref-go-back)
  (keymap-global-set "M-?" #'xref-find-references))

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :branch "main")
  :after eglot
  :config
  ;; Disable bytecode mode for eglot-booster to preserve proper UTF-8 encoding
  (setq eglot-booster-io-only t)
  (eglot-booster-mode))

(provide 'init-eglot)
;;; init-eglot.el ends here
