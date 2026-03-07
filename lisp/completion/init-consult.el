;; -*- coding: utf-8; lexical-binding: t -*-

(eval-and-compile
  (let ((current-file (or load-file-name byte-compile-current-file buffer-file-name)))
    (when current-file
      (add-to-list 'load-path (file-name-directory current-file)))))

;;; init-consult.el --- consult + vertico completion stack

;;; Code:

(use-package consult
  :vc (:url "https://github.com/minad/consult")
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-s" . consult-line-ex)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x r b" . consult-bookmark)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g i" . consult-imenu)
         ;; M-s bindings in `search-map'
         ("M-s g" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Reduce lag while moving candidates in consult-buffer.
  (consult-customize consult-buffer consult-project-buffer
                     :preview-key '(:debounce 0.35 any))
  ;; File candidates are expensive to preview continuously.
  (dolist (source '(consult-source-recent-file
                    consult-source-project-recent-file
                    consult-source-project-recent-file-hidden
                    consult-source-project-root
                    consult-source-project-root-hidden))
    (when (boundp source)
      (setf (plist-get (symbol-value source) :preview-key) nil)))
  :custom
  (consult-buffer-filter '("\` "
                           "\`\*dashboard\*\'"
                           "\`\*img-cache\*\'"
                           "\`\*Warnings\*\'"
                           "\`\*Native-compile-Log\*\'"
                           "\`\*Async-native-compile-log\*\'"
                           "\`\*flymake-popon\*\'"
                           "\`\*Messages\*\'"
                           "\`\*scratch\*\'"
                           "\`\*lsp-bridge"
                           "\`\*Completions\*\'"
                           "\`\*Flymake log\*\'"
                           "\`\*Semantic SymRef\*\'"
                           "\`\*vc\*\'"
                           "\`newsrc-dribble\'"
                           "\`\*tramp/.*\*\'")))

(use-package vertico
  :vc (:url "https://github.com/minad/vertico")
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  :init
  (add-to-list 'load-path (concat repo-dir "vertico/extensions"))
  (vertico-mode))

(use-package vertico-multiform
  :after vertico
  :ensure nil
  :config
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((consult-imenu buffer indexed)
          (execute-extended-command unobtrusive)))
  (setq vertico-multiform-categories
        '((consult-grep buffer))))

;; `vertico-directory` may be unavailable in some environments.
;; Bind safe fallbacks to avoid minibuffer DEL errors.
(with-eval-after-load 'vertico
  (if (require 'vertico-directory nil t)
      (progn
        (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
        (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
        (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
        (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))
    ;; Fallback: keep basic backspace behavior without extension dependency.
    (define-key vertico-map (kbd "DEL") #'backward-delete-char-untabify)))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (setq completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t)
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :vc (:url "https://github.com/minad/marginalia")
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config
  ;; Prioritize cheap file annotations to keep consult-buffer file source smooth.
  (let ((file-entry (assq 'file marginalia-annotator-registry)))
    (when file-entry
      (setcdr file-entry '(builtin marginalia-annotate-file none))))
  (let ((project-file-entry (assq 'project-file marginalia-annotator-registry)))
    (when project-file-entry
      (setcdr project-file-entry '(builtin marginalia-annotate-project-file none))))
  :init
  (marginalia-mode))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-c ;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package prescient
  :config (prescient-persist-mode))

(provide 'init-consult)
