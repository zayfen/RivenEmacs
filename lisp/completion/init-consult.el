;; -*- coding: utf-8; lexical-binding: t -*-

;;; init-consult.el --- consult + vertico completion stack

;;; Code:

(defun riven/consult-elpa-roots ()
  "Return candidate ELPA roots for package fallback loading."
  (let* ((current-file (or load-file-name byte-compile-current-file buffer-file-name))
         (repo-elpa (and current-file
                         (expand-file-name "../../elpa" (file-name-directory current-file))))
         (roots (delq nil
                      (list (and (boundp 'package-user-dir) package-user-dir)
                            (and (boundp 'user-emacs-directory)
                                 (expand-file-name "elpa" user-emacs-directory))
                            (and (boundp 'repo-dir) repo-dir)
                            repo-elpa))))
    (delete-dups roots)))

(defun riven/ensure-elpa-package-load-path (package)
  "Ensure PACKAGE directory in known ELPA roots is in `load-path`."
  (let ((library (symbol-name package)))
    (unless (locate-library library)
      (let* ((pattern (format "^%s\\(?:-[0-9].*\\)?\\'" (regexp-quote library)))
             (selected nil))
        (dolist (root (riven/consult-elpa-roots))
          (when (and (not selected) (file-directory-p root))
            (let ((dirs (directory-files root t pattern t)))
              (when dirs
                (setq selected (car (sort dirs #'string>)))))))
        (when selected
          (add-to-list 'load-path selected))))))

(mapc #'riven/ensure-elpa-package-load-path
      '(consult vertico orderless corfu cape marginalia embark embark-consult prescient nerd-icons-corfu))

(dolist (root (riven/consult-elpa-roots))
  (let ((vertico-ext (expand-file-name "vertico/extensions" root)))
    (when (file-directory-p vertico-ext)
      (add-to-list 'load-path vertico-ext))))

(defun riven/corfu-nerd-icons-formatter (metadata)
  "Return a Corfu margin formatter using nerd-icons with a fallback icon.
METADATA is passed through to `nerd-icons-corfu-formatter` when kind data exists."
  (if (and (fboundp 'nerd-icons-corfu-formatter)
           (plist-get completion-extra-properties :company-kind))
      (nerd-icons-corfu-formatter metadata)
    (let ((fallback (when (fboundp 'nerd-icons-codicon)
                      (nerd-icons-codicon "nf-cod-code" :face 'font-lock-warning-face)))
          (space (propertize " " 'display '(space :width 1))))
      (when fallback
        (lambda (_cand)
          (concat " " fallback space))))))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'riven/corfu-nerd-icons-formatter))

(defun riven/corfu-enable-nerd-icons ()
  "Load `nerd-icons-corfu` and enable its formatter for Corfu margins."
  (if (require 'nerd-icons-corfu nil t)
      (add-to-list 'corfu-margin-formatters #'riven/corfu-nerd-icons-formatter)
    (message "[corfu] nerd-icons-corfu extension unavailable")))

(defun riven/corfu-insert-index-from-key ()
  "Insert Corfu candidate selected by `M-0..M-9` key without pressing RET.
`M-1`..`M-9` insert candidate 1..9 in the current page. `M-0` inserts 10."
  (interactive)
  (let* ((base-key (event-basic-type last-command-event))
         (digit (- base-key ?0))
         (slot (if (zerop digit) 10 digit))
         (target (+ corfu--scroll (1- slot)))
         (page-end (+ corfu--scroll corfu-count)))
    (if (and (>= target 0) (< target corfu--total) (< target page-end))
        (progn
          (corfu--goto target)
          (corfu-insert))
      (message "[corfu] index %d out of current page range" slot))))

(use-package consult
  :vc (:url "https://github.com/minad/consult")
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-s" . consult-line)
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
  ;; Keep lightweight sources on auto preview, but use manual preview for
  ;; expensive file/bookmark sources to keep candidate movement responsive.
  (when (fboundp 'consult--customize-put)
    (consult--customize-put
     '(consult-buffer consult-project-buffer)
     :preview-key
     '"M-.")
    (consult--customize-put
     '(consult-source-bookmark
       consult-source-file-register
       consult-source-recent-file
       consult-source-project-recent-file
       consult-source-project-recent-file-hidden)
     :preview-key
     '"M-."))
  ;; Restore file source states and pin them to manual preview trigger.
  (dolist (source '(consult-source-file-register
                    consult-source-recent-file
                    consult-source-project-recent-file
                    consult-source-project-recent-file-hidden))
    (when (boundp source)
      (setf (plist-get (symbol-value source) :preview-key) "M-.")
      (setf (plist-get (symbol-value source) :state) #'consult--file-state)))
  (when (boundp 'consult-source-bookmark)
    (setf (plist-get (symbol-value 'consult-source-bookmark) :preview-key) "M-.")
    (setf (plist-get (symbol-value 'consult-source-bookmark) :state) #'consult--bookmark-state))
  :custom
  (consult-buffer-filter '("\\` "
                           "\\`\\*.*\\*\\'")))

(use-package vertico
  :vc (:url "https://github.com/minad/vertico")
  :demand t
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  :config
  (vertico-mode))

(use-package vertico-multiform
  :if (locate-library "vertico-multiform")
  :after vertico
  :ensure nil
  :config
  (require 'vertico-buffer nil t)
  (require 'vertico-indexed nil t)
  (require 'vertico-unobtrusive nil t)
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((consult-imenu buffer indexed)))
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
  :demand t
  :init
  (savehist-mode))

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  (completion-cycle-threshold 3)
  (completion-cycling t)
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
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :ensure t
  :demand t
  :custom
  (tab-always-indent 'complete)
  (corfu-on-exact-match 'insert)
  (corfu-auto t)
  (corfu-auto-delay 0.12)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-count 9)
  (corfu-preselect 'first)
  (corfu-preview-current t)
  (corfu-min-width 48)
  (corfu-max-width 120)
  (corfu-scroll-margin 2)
  :bind (:map corfu-map
              ("M-0" . riven/corfu-insert-index-from-key)
              ("M-1" . riven/corfu-insert-index-from-key)
              ("M-2" . riven/corfu-insert-index-from-key)
              ("M-3" . riven/corfu-insert-index-from-key)
              ("M-4" . riven/corfu-insert-index-from-key)
              ("M-5" . riven/corfu-insert-index-from-key)
              ("M-6" . riven/corfu-insert-index-from-key)
              ("M-7" . riven/corfu-insert-index-from-key)
              ("M-8" . riven/corfu-insert-index-from-key)
              ("M-9" . riven/corfu-insert-index-from-key)
              ("M-P" . corfu-scroll-down)
              ("M-N" . corfu-scroll-up))
  :config
  (global-corfu-mode 1)
  (if (require 'corfu-indexed nil t)
      (progn
        (setq corfu-indexed-start 1)
        (corfu-indexed-mode 1))
    (message "[corfu] corfu-indexed extension unavailable"))
  (riven/corfu-enable-nerd-icons))

(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :bind (:map corfu-map
              ("M-h" . corfu-popupinfo-documentation))
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-max-width 100)
  (corfu-popupinfo-max-height 14)
  :config
  (corfu-popupinfo-mode 1))

(use-package cape
  :ensure t
  :config
  ;; Keep LSP completion first and append generic CAPFs as fallbacks.
  (dolist (capf '(cape-file cape-dabbrev cape-keyword cape-symbol))
    ;; Remove stale CAPF symbols to avoid `void-function` errors when cape is missing.
    (setq completion-at-point-functions (delq capf completion-at-point-functions))
    (when (fboundp capf)
      (add-hook 'completion-at-point-functions capf t)))
  (with-eval-after-load 'eglot
    ;; When eglot activates, pin the CAPF order: LSP first, dabbrev/file as fallback.
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (when (bound-and-true-p eglot--managed-mode)
                  (setq-local completion-at-point-functions
                              (list #'eglot-completion-at-point
                                    #'cape-dabbrev
                                    #'cape-file)))))
    ;; cape-wrap-buster calls the CAPF and wraps the result; it must be used
    ;; as :around advice so it is evaluated lazily at completion time, not
    ;; eagerly at setup time.  This ensures fresh LSP results on each keystroke.
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))

(use-package marginalia
  :vc (:url "https://github.com/minad/marginalia")
  :demand t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config
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
  :demand t
  :config (prescient-persist-mode))

(provide 'init-consult)
