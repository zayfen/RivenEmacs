;; -*- coding: utf-8; lexical-binding: t -*-

;;; init-minibuffer.el --- Minibuffer completion stack

;;; Code:

(defun riven/completion-elpa-roots ()
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

(defun riven/completion-ensure-elpa-package-load-path (package)
  "Ensure PACKAGE directory in known ELPA roots is in `load-path'."
  (let ((library (symbol-name package)))
    (unless (locate-library library)
      (let* ((pattern (format "^%s\\(?:-[0-9].*\\)?\\'" (regexp-quote library)))
             (selected nil))
        (dolist (root (riven/completion-elpa-roots))
          (when (and (not selected) (file-directory-p root))
            (let ((dirs (directory-files root t pattern t)))
              (when dirs
                (setq selected (car (sort dirs #'string>)))))))
        (when selected
          (add-to-list 'load-path selected))))))

(mapc #'riven/completion-ensure-elpa-package-load-path
      '(consult vertico orderless corfu cape marginalia embark embark-consult
        prescient nerd-icons-corfu))

(dolist (root (riven/completion-elpa-roots))
  (let ((vertico-ext (expand-file-name "vertico/extensions" root)))
    (when (file-directory-p vertico-ext)
      (add-to-list 'load-path vertico-ext))))

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

(with-eval-after-load 'vertico
  (if (require 'vertico-directory nil t)
      (progn
        (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
        (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
        (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
        (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))
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

(use-package marginalia
  :vc (:url "https://github.com/minad/marginalia")
  :demand t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

(use-package prescient
  :demand t
  :config (prescient-persist-mode))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
