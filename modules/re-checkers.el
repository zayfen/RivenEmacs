;;; re-checkers.el --- Syntax checking -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")



;; (use-package flymake
;;   :straight (:type built-in)
;;   :init
;;   (+map! "tf" #'flymake-mode)
;;   :hook ((python-mode . flymake-mode)
;;          (python-ts-mode . flymake-mode)
;;          (javascript-mode . flymake-mode)
;;          (web-mode . flymake-mode)
;;          (typescript-ts-mode . flymake-mode)
;;          (tsx-ts-mode . flymake-mode)
;;          (rust-mode . flymake-mode)
;;          (rust-ts-mode . flymake-mode)
;;          (c-ts-mode . flymake-mode)
;;          (c-mode . flymake-mode)
;;          (c++-mode . flymake-mode)
;;          (c++-ts-mode . flymake-mode))
;;   :custom
;;   (flymake-fringe-indicator-position 'right-fringe)
;;   (flymake-error-bitmap '(+flymake-bitmap-left-arrow-hi-res compilation-error))
;;   (flymake-warning-bitmap '(+flymake-bitmap-left-arrow-hi-res compilation-warning))
;;   (flymake-note-bitmap '(+flymake-bitmap-left-arrow-hi-res compilation-info))
;;   :config
;;   (+map! :keymaps 'flymake-mode-map
;;     "!"  '(nil :wk "flymake")
;;     "!n" #'flymake-goto-next-error
;;     "!p" #'flymake-goto-prev-error
;;     "!s" #'flymake-start
;;     "!d" #'flymake-show-buffer-diagnostics
;;     "!D" #'flymake-show-project-diagnostics)
;;   ;; remap next-error and previous-error
;;   ;;(global-set-key [remap next-error] #'flymake-goto-next-error)
;;   (define-key flymake-mode-map [remap next-error] #'flymake-goto-next-error)
;;   ;; (global-set-key [remap previous-error] #'flymake-goto-prev-error)
;;   (define-key flymake-mode-map [remap previous-error] #'flymake-goto-prev-error)

;;   ;; Use the session's load-path with flymake
;;   (setq elisp-flymake-byte-compile-load-path load-path)
;;   ;; Larger right frings
;;   (set-fringe-style '(8 . 13))

;;   ;; Better fringe bitmaps
;;   (when (fboundp 'define-fringe-bitmap)
;;     (define-fringe-bitmap '+flymake-bitmap-arrow
;;       [#b11111000
;;        #b01111100
;;        #b00111110
;;        #b00011111
;;        #b00111110
;;        #b01111100
;;        #b11111000])
;;     (define-fringe-bitmap '+flymake-bitmap-arrow-hi-res
;;       [#b01111000000
;;        #b00111100000
;;        #b00011110000
;;        #b00001111000
;;        #b00000111100
;;        #b00000011110
;;        #b00000011110
;;        #b00000111100
;;        #b00001111000
;;        #b00011110000
;;        #b00111100000
;;        #b01111000000]
;;       nil 13)
;;     (define-fringe-bitmap '+flymake-bitmap-left-arrow-hi-res
;;       [#b00000011110
;;        #b00000111100
;;        #b00001111000
;;        #b00011110000
;;        #b00111100000
;;        #b01111000000
;;        #b01111000000
;;        #b00111100000
;;        #b00011110000
;;        #b00001111000
;;        #b00000111100
;;        #b00000011110]
;;       nil 13)))

;; (use-package flymake-easy
;;   :straight t)


;; use flycheck
;; Fixups

(use-package flycheck
  :ensure t
  :if (version<= "24.4" emacs-version)
  :commands flycheck-mode
  :hook (prog-mode . flycheck-mode)

  :config
  (setq-default flycheck-emacs-lisp-initialize-packages t
                flycheck-highlighting-mode 'lines
                )
  (define-key flycheck-mode-map [remap next-error] #'flycheck-next-error)
  (define-key flycheck-mode-map [remap previous-error] #'flycheck-previous-error))


;; Other pkgs
(use-package flycheck-tip
  :ensure t
  :commands 'flycheck-tip-cycle
  :after flycheck
  :bind (:map flycheck-mode-map
              ("C-c C-n" . flycheck-tip-cycle)))

(use-package flycheck-package
  :ensure t)

(use-package flycheck-checkpatch
  :ensure t
  :config (flycheck-checkpatch-setup)
  :config (setq flycheck-checkers (delete 'checkpatch flycheck-checkers))
  :config (add-to-list 'flycheck-checkers 'checkpatch t))

(use-package flycheck-inline
  :ensure t
  :after flycheck
  :config
  (setq flycheck-inline-display-function
      (lambda (msg pos err)
        (let* ((ov (quick-peek-overlay-ensure-at pos))
               (contents (quick-peek-overlay-contents ov)))
          (setf (quick-peek-overlay-contents ov)
                (concat contents (when contents "\n") msg))
          (quick-peek-update ov)))
      flycheck-inline-clear-function #'quick-peek-hide))

(use-package flycheck-rust
  :ensure t
  :after flycheck
  :commands flycheck-rust-setup
  :hook ((rust-mode . flycheck-rust-setup)
         (rust-ts-mode . flycheck-rust-setup)))

(provide 're-checkers)
