;;; -*- coding: utf-8; lexical-binding: t -*-

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

(defun riven/eglot-python-contact ()
  "Return preferred Eglot server contact for Python."
  (cond
   ((executable-find "basedpyright-langserver")
    '("basedpyright-langserver" "--stdio"))
   ((executable-find "pyright-langserver")
    '("pyright-langserver" "--stdio"))
   ((executable-find "ruff")
    '("ruff" "server"))
   (t '("pylsp"))))

(defun riven/eglot-configure-python-server ()
  "Configure Python language server selection for Eglot."
  (when (boundp 'eglot-server-programs)
    (let ((contact (riven/eglot-python-contact)))
      (setf (alist-get 'python-mode eglot-server-programs nil nil #'eq) contact)
      (setf (alist-get 'python-ts-mode eglot-server-programs nil nil #'eq) contact))))

(defun riven/eglot-client-capabilities-no-file-watch (orig-fn server)
  "Call ORIG-FN for SERVER, disabling dynamic file-watch registration."
  (let* ((caps (funcall orig-fn server))
         (workspace (plist-get caps :workspace))
         (watch (and (listp workspace)
                     (plist-get workspace :didChangeWatchedFiles))))
    (when (listp watch)
      (setq watch (plist-put watch :dynamicRegistration :json-false))
      (setq workspace (plist-put workspace :didChangeWatchedFiles watch))
      (setq caps (plist-put caps :workspace workspace)))
    caps))

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
  (with-eval-after-load 'eglot
    (unless (advice-member-p #'riven/eglot-client-capabilities-no-file-watch
                             #'eglot-client-capabilities)
      (advice-add #'eglot-client-capabilities :around
                  #'riven/eglot-client-capabilities-no-file-watch)))
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
  (riven/eglot-configure-python-server)
  (keymap-global-set "M-." #'riven/xref-find-definitions-or-search)
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
