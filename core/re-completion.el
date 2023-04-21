;;; completion.el --- Completion packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;;###autoload
(defun +vertico/embark-export-write ()
  "Export the current vertico results to a writable buffer if possible.

Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (let* ((edit-command
          (pcase-let ((`(,type . ,candidates)
                       (run-hook-with-args-until-success 'embark-candidate-collectors)))
            (pcase type
              ('consult-grep #'wgrep-change-to-wgrep-mode)
              ('file #'wdired-change-to-wdired-mode)
              ('consult-location #'occur-edit-mode)
              (x (user-error "embark category %S doesn't support writable export" x)))))
         (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
    (embark-export)))

;;;###autoload
(defun +vertico/embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (save-selected-window
      (let ((embark-quit-after-action nil))
        (embark-dwim)))))

(defvar +vertico/find-file-in--history nil)
;;;###autoload
(defun +vertico/find-file-in (&optional dir initial)
  "Jump to file under DIR (recursive).
If INITIAL is non-nil, use as initial input."
  (interactive)
  (require 'consult)
  (let* ((default-directory (or dir default-directory))
         (prompt-dir (consult--directory-prompt "Find" default-directory))
         (cmd (split-string-and-unquote +vertico-consult-fd-args " ")))
    (find-file
     (consult--read
      (split-string (cdr (apply #'doom-call-process cmd)) "\n" t)
      :prompt default-directory
      :sort nil
      :initial (if initial (shell-quote-argument initial))
      :add-history (thing-at-point 'filename)
      :category 'file
      :history '(:input +vertico/find-file-in--history)))))

;;;###autoload
(defun +vertico/jump-list (jump)
  "Go to an entry in evil's (or better-jumper's) jumplist."
  (interactive
   (let (buffers)
     (require 'consult)
     (unwind-protect
         (list
          (consult--read
           ;; REVIEW Refactor me
           (nreverse
            (delete-dups
             (delq
              nil (mapcar
                   (lambda (mark)
                     (when mark
                       (cl-destructuring-bind (path pt _id) mark
                         (let* ((visiting (find-buffer-visiting path))
                                (buf (or visiting (find-file-noselect path t)))
                                (dir default-directory))
                           (unless visiting
                             (push buf buffers))
                           (with-current-buffer buf
                             (goto-char pt)
                             (font-lock-fontify-region
                              (line-beginning-position) (line-end-position))
                             (format "%s:%d: %s"
                                     (car (cl-sort (list (abbreviate-file-name (buffer-file-name buf))
                                                         (file-relative-name (buffer-file-name buf) dir))
                                                   #'< :key #'length))
                                     (line-number-at-pos)
                                     (string-trim-right (or (thing-at-point 'line) ""))))))))
                   (cddr (better-jumper-jump-list-struct-ring
                          (better-jumper-get-jumps (better-jumper--get-current-context))))))))
           :prompt "jumplist: "
           :sort nil
           :require-match t
           :category 'jump-list))
       (mapc #'kill-buffer buffers))))
  (if (not (string-match "^\\([^:]+\\):\\([0-9]+\\): " jump))
      (user-error "No match")
    (let ((file (match-string-no-properties 1 jump))
          (line (match-string-no-properties 2 jump)))
      (find-file file)
      (goto-char (point-min))
      (forward-line (string-to-number line)))))

(use-package consult
  :straight t
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :custom
  ;; Use `consult-xref' for `xref-find-references'
  (xref-show-xrefs-function #'consult-xref)
  ;; Better formatting for `view-register'
  (register-preview-function #'consult-register-format)
  :preface
  (define-key!
    [remap bookmark-jump]                 #'consult-bookmark
    [remap evil-show-marks]               #'consult-mark
    [remap evil-show-jumps]               #'+vertico/jump-list
    [remap evil-show-registers]           #'consult-register
    [remap goto-line]                     #'consult-goto-line
    [remap imenu]                         #'consult-imenu
    [remap Info-search]                   #'consult-info
    [remap locate]                        #'consult-locate
    [remap load-theme]                    #'consult-theme
    [remap man]                           #'consult-man
    [remap recentf-open-files]            #'consult-recent-file
    [remap switch-to-buffer]              #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
    [remap yank-pop]                      #'consult-yank-pop)

  :init
  (define-key minibuffer-local-map (kbd "C-r") #'consult-history)
  (define-key minibuffer-local-map (kbd "S-C-v") #'consult-yank-pop)
  (global-set-key (kbd "C-s") #'consult-line)
  (+map!
    ;; buffer
    "bl"  #'consult-line
    "bb"  #'consult-buffer
    "bmM" #'consult-bookmark
    "bO"  #'consult-outline
    ;; file
    "fe"  '(consult-recent-file :wk "Recent files")
    "fl"  #'consult-locate
    "fd"  '(consult-find :wk "Find file in directory")

    ;; git/vc
    "gG"  #'consult-git-grep
    ;; search
    "s."  #'consult-ripgrep
    "si"  #'consult-imenu
    "sm"  #'consult-man
    "sh"  #'consult-history
    "sa"  #'consult-org-agenda

    ;; project
    "pl"  #'consult-line-multi
    "pi"  #'consult-imenu-multi
    ;; code
    "cE"  #'consult-compile-error
    ;; unclassified
    "xc"  #'consult-complex-command
    ;; insert
    "iy"  #'consult-yank-from-kill-ring
    "ir"  '(nil :wk "register")
    "irr" #'consult-register
    "irl" #'consult-register-load
    "irs" #'consult-register-store
    ;; help
    "hu"  #'consult-theme
    "hI"  #'consult-info)

  (+map-local! :keymaps 'org-mode-map
    "h"   #'consult-org-heading)
  :config
  (defadvice! +vertico--consult-recent-file-a (&rest _args)
    "`consult-recent-file' needs to have `recentf-mode' on to work correctly"
    :before #'consult-recent-file
    (recentf-mode +1))

  (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)

  (setq-default completion-in-region-function #'consult-completion-in-region)

  ;; TWEAK: Fill the `initial' query of `consult' commands from
  ;; `thing-at-point'.
  ;; NOTE: Some `consult' commands have slightly different signature, the
  ;; `initial' argument can come first in some cases (like `consult-line') or
  ;; second in some other cases (like `condult-grep'). These two advices are
  ;; added to such family of commands so it is filled in the right place.
  (dolist (cmd '(consult-line ; `initial' comes first in these commands
                 consult-man))
    (advice-add
     cmd :around
     (defun +consult--dwim-first-arg-a (orig-fn &optional initial opt)
       (apply orig-fn
              (append
               (if (and (called-interactively-p) (not (minibufferp)))
                   (list (or initial (+region-or-thing-at-point)))
                 (list initial))
               (when opt (list opt)))))))

  (dolist (cmd '(consult-ripgrep ; `initial' comes second in these commands
                 consult-line-multi
                 consult-grep
                 consult-find))
    (advice-add
     cmd :around
     (defun +consult--dwim-second-arg-a (orig-fn &optional dir initial)
       (apply orig-fn
              (append
               (list dir)
               (if (and (called-interactively-p) (not (minibufferp)))
                   (list (or initial (+region-or-thing-at-point)))
                 (list initial))))))))

(use-package embark
  :straight t
  :init
  (global-set-key [remap describe-bindings] #'embark-bindings)
  (setq prefix-help-command #'embark-prefix-help-command)
  (+map! "." #'embark-act))

(use-package embark-consult
  :straight t
  :after embark consult
  :demand t)

(use-package marginalia
  :straight t
  :hook (rivenemacs-after-startup . marginalia-mode))

(use-package all-the-icons-completion
  :straight t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package orderless
  :straight t
  :after rivenemacs-loaded
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :straight t
  :hook (rivenemacs-after-startup . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 12)
  :init
  (add-to-list
   'load-path (concat
               straight-base-dir
               (format "straight/%s/vertico/extensions" straight-build-dir)))
  ;; In the minibuffer, "C-k" is be mapped to act like "<up>". However, in
  ;; Emacs, "C-k" have a special meaning of `kill-line'. So lets map "C-S-k"
  ;; to serve the original "C-k".
  (define-key minibuffer-local-map (kbd "C-k") #'kill-line))


(use-package vertico-directory
  :after vertico
  :demand t
  :config
  (define-key vertico-map "\r" #'vertico-directory-enter)
  (define-key vertico-map "\d" #'vertico-directory-delete-char)
  (define-key vertico-map "\M-\d" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))


(use-package vertico-repeat
  :hook (minibuffer-setup . vertico-repeat-save)
  :init
  (keymap-global-set "M-R" #'vertico-repeat))


(use-package blink-search
  :straight (:host github :repo "manateelazycat/blink-search" :files ("*" (:exclude ".git")))
  :commands (blink-search)
  :init
  (setq blink-search-browser-function
        (if (display-graphic-p)
            #'xwidget-webkit-browse-url
          #'eww))
  (+map! :infix "s"
    "s" '(blink-search :wk "Blink Search")))


(use-package color-rg
  :straight (:host github :repo "manateelazycat/color-rg" :files ("*" (:exclude ".git")))
  :commands (color-rg-search-input-in-project color-rg-search-symbol-in-project color-rg-search-input-in-current-file color-rg-search-symbol-in-current-file)
  :init
  (+map! :infix "s"
    "p" '(color-rg-search-symbol-in-project :wk "Search project at point")
    "P" '(color-rg-search-input-in-project :wk "Search project")
    "b" '(color-rg-search-symbol-in-current-file :wk "Search buffer at point")
    "B" '(color-rg-search-input-in-current-file :wk "Search buffer")))


(use-package find-file-in-project
  :straight t
  :init
  (+map! :infix "f"
    "." 'find-file-in-project-at-point
    "g" 'find-file-in-project-by-selected
    "f" 'find-file-in-project))



(provide 're-completion)
