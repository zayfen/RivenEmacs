;;; -*- coding: utf-8; lexical-binding: t -*-

(eval-and-compile
  (let ((current-file (or load-file-name byte-compile-current-file buffer-file-name)))
    (when current-file
      (add-to-list 'load-path (file-name-directory current-file)))))

(require 'init-config)

(defcustom rivenEmacs-eglot-use-booster t
  "Whether to enable `emacs-lsp-booster` acceleration for Eglot."
  :type 'boolean
  :group 'rivenEmacs)

(defun riven/eglot-booster-available-p ()
  "Return non-nil when `emacs-lsp-booster` is available for local projects."
  (and rivenEmacs-eglot-use-booster
       (executable-find "emacs-lsp-booster")
       (not (file-remote-p default-directory))))

(defun riven/eglot-booster--jsonrpc-json-read (old-fn &rest args)
  "Decode booster bytecode payload, otherwise delegate to OLD-FN with ARGS."
  (let ((start (point)))
    (or
     (when (eq (following-char) ?#)
       (condition-case nil
           (let ((bytecode (read (current-buffer))))
             (when (byte-code-function-p bytecode)
               (funcall bytecode)))
         (error
          (goto-char start)
          nil)))
     (apply old-fn args))))

(defun riven/eglot-booster--eglot-cmd (old-fn contact)
  "Prefix CONTACT command from OLD-FN with `emacs-lsp-booster` when possible."
  (let ((command (funcall old-fn contact)))
    (if (and (riven/eglot-booster-available-p)
             (listp command)
             (stringp (car command))
             (not (string-equal (file-name-nondirectory (car command))
                                "emacs-lsp-booster")))
        (cons "emacs-lsp-booster" command)
      command)))

(defun riven/eglot-booster-enable ()
  "Enable `emacs-lsp-booster` advices for Eglot/jsonrpc.
Return non-nil when advices are installed."
  (when (riven/eglot-booster-available-p)
    (unless (advice-member-p #'riven/eglot-booster--jsonrpc-json-read #'jsonrpc--json-read)
      (advice-add 'jsonrpc--json-read :around #'riven/eglot-booster--jsonrpc-json-read))
    (unless (advice-member-p #'riven/eglot-booster--eglot-cmd #'eglot--cmd)
      (advice-add 'eglot--cmd :around #'riven/eglot-booster--eglot-cmd))
    t))

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
  (eglot-sync-connect nil)
  (eglot-autoshutdown t)
  (eglot-report-progress nil)
  (eglot-send-changes-idle-time 0.2)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-config '(:size 0 :format short))
  :config
  (keymap-global-set "M-." #'xref-find-definitions)
  (keymap-global-set "M-," #'xref-go-back)
  (keymap-global-set "M-?" #'xref-find-references)
  (unless (riven/eglot-booster-enable)
    (message "[eglot] emacs-lsp-booster not found, fallback to plain eglot")))

(provide 'init-eglot)
;;; init-eglot.el ends here
