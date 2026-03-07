;; -*- coding: utf-8; lexical-binding: t -*-

(eval-and-compile
  (let ((current-file (or load-file-name byte-compile-current-file buffer-file-name)))
    (when current-file
      (add-to-list 'load-path (file-name-directory current-file)))))

;;; init-consult.el --- consult + vertico completion stack

;;; Code:

(defun riven/ensure-elpa-package-load-path (package)
  "Ensure PACKAGE version directory in ELPA is present in `load-path`.
This is a fallback for broken/missing autoload files in package metadata."
  (let ((library (symbol-name package)))
    (unless (locate-library library)
      (let* ((elpa-root (if (boundp 'package-user-dir)
                            package-user-dir
                          (expand-file-name "elpa" user-emacs-directory)))
             (pattern (format "^%s-[0-9]" (regexp-quote library)))
             (candidates (and (file-directory-p elpa-root)
                              (directory-files elpa-root t pattern t))))
        (when candidates
          (add-to-list 'load-path (car (sort candidates #'string>))))))))

(riven/ensure-elpa-package-load-path 'corfu)
(riven/ensure-elpa-package-load-path 'cape)
(add-to-list 'load-path (expand-file-name "elpa/vertico/extensions" user-emacs-directory))

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
                     :preview-key '(:debounce 0.2 any))
  ;; File candidates are expensive to preview continuously.
  (dolist (source '(consult-source-recent-file
                    consult-source-project-recent-file
                    consult-source-project-recent-file-hidden
                    consult-source-project-root
                    consult-source-project-root-hidden))
    (when (boundp source)
      (setf (plist-get (symbol-value source) :preview-key) nil)))
  :custom
  (consult-buffer-filter '("\\` "
                           "\\`\\*.*\\*\\'")))

(use-package vertico
  :vc (:url "https://github.com/minad/vertico")
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  :init
  (vertico-mode))

(use-package vertico-multiform
  :if (locate-library "vertico-multiform")
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
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-count 14)
  (corfu-preselect 'prompt)
  (corfu-preview-current t)
  (corfu-min-width 48)
  (corfu-max-width 120)
  (corfu-scroll-margin 2)
  :bind (:map corfu-map
              ("M-P" . corfu-scroll-down)
              ("M-N" . corfu-scroll-up))
  :config
  (global-corfu-mode 1))

(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :init
  (defun riven/corfu-popupinfo-safe-show (orig-fn &rest args)
    "Call ORIG-FN with ARGS, suppressing known JSON type errors."
    (condition-case err
        (apply orig-fn args)
      (wrong-type-argument
       (if (eq (nth 1 err) 'json-value-p)
           nil
         (signal (car err) (cdr err))))))
  :bind (:map corfu-map
              ("M-h" . corfu-popupinfo-documentation))
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-max-width 100)
  (corfu-popupinfo-max-height 14)
  :config
  (unless (advice-member-p #'riven/corfu-popupinfo-safe-show #'corfu-popupinfo--show)
    (advice-add 'corfu-popupinfo--show :around #'riven/corfu-popupinfo-safe-show))
  (corfu-popupinfo-mode 1))

(use-package cape
  :ensure t
  :init
  ;; Keep LSP completion first and append generic CAPFs as fallbacks.
  (add-hook 'completion-at-point-functions #'cape-file t)
  (add-hook 'completion-at-point-functions #'cape-dabbrev t)
  (add-hook 'completion-at-point-functions #'cape-keyword t)
  (add-hook 'completion-at-point-functions #'cape-symbol t)
  :config
  (with-eval-after-load 'eglot
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))

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
