;;; re-prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")



  ;; Use built-in `treesit' when available
  (use-package treesit
    :straight (:type built-in)
    :custom
    (treesit-font-lock-level 3))

  (use-package treesit-auto
    :straight (:host github :repo "renzmann/treesit-auto")
    :hook (rivenemacs-after-startup . global-treesit-auto-mode)
    :custom
    (treesit-auto-install 'prompt)
    :config
    ;; Install all languages when calling `treesit-auto-install-all'
    (setq treesit-language-source-alist (treesit-auto--build-treesit-source-alist)))

(use-package hideif
  :straight (:type built-in)
  :init
  ;; If `re-lsp' is enabled, `lsp-semantic-tokens-mode' should do a better job,
  ;; so we don't enable `hide-ifdef-mode'.
  (unless (memq 're-lsp rivenemacs-modules)
    (dolist (h '(c++-mode-hook c++-ts-mode-hook c-mode-hook c-ts-mode-hook cuda-mode-hook))
      (add-hook h #'hide-ifdef-mode)))
  :custom
  (hide-ifdef-shadow t)
  (hide-ifdef-initially t))

(use-package eldoc
  :straight (:type built-in)
  :custom
  (eldoc-documentation-strategy #'eldoc-documentation-compose))

(use-package eldoc-box
  :straight t
  :hook (prog-mode . eldoc-box-hover-at-point-mode)
  :hook (lsp-bridge-mode . eldoc-box-hover-at-point-mode)
  :hook ((tab-bar-mode tool-bar-mode) . +eldoc-box-hover-at-point-fix-h)
  :config
  ;; HACK: Temporary fix for `eldoc-box-hover-at-point-mode' with `tab-bar-mode'
  ;; and `tool-bar-mode'.
  (defun +eldoc-box-hover-at-point-fix-h ()
    (when (bound-and-true-p eldoc-box-hover-at-point-mode)
      (eldoc-box-hover-at-point-mode -1)
      (eldoc-box-hover-at-point-mode 1))))

(use-package cov
  :straight (:host github :repo "abougouffa/cov" :branch "feat/gcov-cmake")
  :custom
  (cov-highlight-lines t)
  :config
  (defun +cov-coverage-mode ()
    (interactive)
    (if cov-coverage-mode
        (progn
          (setq cov-coverage-mode nil)
          (message "Disabled coverage mode, showing how often lines are executed."))
      (setq cov-coverage-mode t)
      (message "Enabled coverage mode."))
    (cov-update)))

(use-package compile-multi
  :straight t
  :commands +project-compile-multi
  :init
  (+map! "pC" #'+project-compile-multi)
  :config
  (defun +project-compile-multi ()
    "Like `project-compile', but uses `compile-multi'."
    (declare (interactive-only compile))
    (interactive)
    (let ((default-directory (project-root (project-current t)))
          (compilation-buffer-name-function
           (or project-compilation-buffer-name-function
               compilation-buffer-name-function)))
      (call-interactively #'compile-multi))))

(use-package compile
  :straight (:type built-in)
  :commands +toggle-burry-compilation-buffer-if-successful
  ;; Enable ANSI colors in compilation buffer
  :hook (compilation-filter . ansi-color-compilation-filter)
  :config
  ;; Integration of `compile' with `savehist'
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'compile-history))

  ;; Auto-close the compilation buffer if succeeded without warnings.
  ;; Adapted from: stackoverflow.com/q/11043004/3058915
  (defun +compilation--bury-if-successful-h (buf str)
    "Bury the compilation buffer if it succeeds without warnings."
    (when (and
           (string-match "compilation" (buffer-name buf))
           (string-match "finished" str)
           (not (with-current-buffer buf
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward "warning" nil t)))))
      (run-with-timer
       3 nil
       (lambda (b)
         (with-selected-window (get-buffer-window b)
           (kill-buffer-and-window))
         (unless (current-message)
           (message "Compilation finished without warnings.")))
       buf)))

  (defun +toggle-burry-compilation-buffer-if-successful ()
    "Toggle auto-burying the successful compilation buffer."
    (interactive)
    (if (memq '+compilation--bury-if-successful-h compilation-finish-functions)
        (progn
          (message "Disabled burying compilation buffer.")
          (remove-hook 'compilation-finish-functions #'+compilation--bury-if-successful-h))
      (message "Enabled burying compilation buffer.")
      (add-hook 'compilation-finish-functions #'+compilation--bury-if-successful-h))))

(use-package apheleia
  :straight t
  :init
  (+map! "cff" #'apheleia-format-buffer))

(use-package apheleia-formatters
  :config
  (add-to-list 'apheleia-formatters '(cmake-format . ("cmake-format")))
  ;; TEMP: Use the `tab-width' value for `shfmt' formatting. Delete this hack if
  ;; this PR github.com/radian-software/apheleia/pull/179 gets merged.
  (+alist-set 'shfmt '("shfmt" "-i" (number-to-string tab-width)) apheleia-formatters)
  (dolist (alist '((cmake-mode . cmake-format)
                   (cmake-ts-mode . cmake-format)
                   (lisp-data-mode . lisp-indent)
                   (sh-mode . shfmt)
                   (emacs-lisp-mode . lisp-indent)))
    (add-to-list 'apheleia-mode-alist alist)))

(use-package editorconfig
  :straight t
  :ensure t
  :hook (prog-mode . editorconfig-mode)
  :init
  (+map!
    "fc" '(editorconfig-find-current-editorconfig :wk "Find current EditorConfig")
    "cfe" #'editorconfig-format-buffer)
  :config (editorconfig-mode 1))

(use-package clang-format
  :straight t
  :init
  (+map! :keymaps '(c-mode-map c++-mode-map cuda-mode-map scad-mode-map)
    "cfc" #'clang-format-buffer))

;;; Modes
;; (use-package vimrc-mode
;;   :straight t
;;   :mode "\\.vim\\(rc\\)?\\'")

(use-package cmake-mode
  :straight (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*"))
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'")

(use-package cmake-font-lock
  :straight (:host github :repo "Lindydancer/cmake-font-lock" :files (:defaults "*"))
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package cuda-mode
  :straight t
  :hook (cuda-mode . display-line-numbers-mode)
  :hook (cuda-mode . hs-minor-mode))

(use-package opencl-mode
  :straight t
  :mode "\\.cl\\'")

(use-package dumb-jump
  :straight t
  :commands
  +dumb-jump-hydra/body
  :after xref
  :custom
  (dumb-jump-selector 'completing-read)
  :init
  ;; remove etag feature and only use dumb-jump as xref-backend
  (setq xref-backend-functions (remq 'etags--xref-backend xref-backend-functions))
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate t)

  (+map!
    "j" '(+dumb-jump-hydra/body :wk "+dumb-jump-hydra"))

  :config
  ;; Define Hydra keybinding (from the repo's examples)
  (defhydra +dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump."
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")))

(use-package xref
  :straight (:type built-in)
 )


(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo")
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        (append
         hl-todo-keyword-faces
         '(("BUG" . "#ee5555")
           ("PROJ" . "#447f44")
           ("IDEA" . "#0fa050")
           ("INFO" . "#0e9030")
           ("TWEAK" . "#fe9030")
           ("PERF" . "#e09030")))))

(use-package rainbow-mode
  :straight t
  :init
  (+map! :keymaps '(prog-mode-map conf-mode-map text-mode-map)
    "tR" #'rainbow-mode))

(use-package lua-mode
  :straight t
  :custom
  (lua-indent-level 2))

(use-package powershell
  :straight t)

(use-package franca-idl
  :straight (:host github :repo "zeph1e/franca-idl.el"))

(use-package bnf-mode
  :straight t)

(use-package ebnf-mode
  :straight (:host github :repo "jeramey/ebnf-mode")
  :hook (ebnf-mode . display-line-numbers-mode)
  :mode "\\.ebnf\\'")

;; avy-goto-char
(use-package avy
  :ensure t
  :bind ("C-:" . avy-goto-char-timer))

;; code fold
(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :init
  (+map! "@ TAB" #'ts-fold-toggle))

(use-package ts-fold-indicators
  :straight (ts-fold-indicators :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :init (add-hook 'tree-sitter-after-on-hook #'ts-fold-indicators-mode))


(defvar-local +electric-indent-words '()
  "The list of electric words. Typing these will trigger reindentation of the
current line.")

;;
(after! electric
  (setq-default electric-indent-chars '(?\n ?\^?))

  (add-hook! 'electric-indent-functions
    (defun +electric-indent-char-fn (_c)
      (when (and (eolp) +electric-indent-words)
        (save-excursion
          (backward-word)
          (looking-at-p (concat "\\<" (regexp-opt +electric-indent-words))))))))


(provide 're-prog)

;;; re-prog.el ends here
