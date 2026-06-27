;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-treesit.el --- Config for treesit

;;; Commentary:
;; Emacs 31 provides native tree-sitter major-mode remapping via
;; `treesit-enabled-modes' and `treesit-major-mode-remap-alist', which
;; replaces the former `treesit-auto' package and the hand-maintained
;; `major-mode-remap-alist'. We therefore only keep the grammar source
;; list, an install helper, and the per-mode parser eager-creation.

;;; Require
(require 'treesit)

;;; Code:

;; M-x `treesit-install-language-grammar` to install language grammar.
(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (make . ("https://github.com/alemuller/tree-sitter-make"))
        (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "grammars/ocaml/src"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (php . ("https://github.com/tree-sitter/tree-sitter-php"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
        (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
        (zig . ("https://github.com/GrayJack/tree-sitter-zig"))
        (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell"))
        (kotlin . ("https://github.com/fwcd/tree-sitter-kotlin"))
        (java . ("https://github.com/tree-sitter/tree-sitter-java"))
        (lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (elixir . ("https://github.com/elixir-lang/tree-sitter-elixir"))
        (heex . ("https://github.com/phoenixframework/tree-sitter-heex"))
        (angular . ("https://github.com/dlvandenberg/tree-sitter-angular"))
        (swift . ("https://github.com/alex-pinkus/tree-sitter-swift"))))

;; Emacs 31 native tree-sitter activation. Setting `treesit-enabled-modes'
;; to t populates `major-mode-remap-alist' from the built-in
;; `treesit-major-mode-remap-alist' for every available ts-mode, replacing
;; both the `treesit-auto' package and the former hand-written remap alist.
;;
;; NOTE: `treesit-enabled-modes' uses a custom `:set' setter, so it must be
;; assigned with `customize-set-variable' (or `setopt') — a plain `setq'
;; sets the value but never runs the remap population.
(customize-set-variable 'treesit-enabled-modes t)

;; `treesit-auto-install-grammar' (NEWS.31) auto-installs grammars when a
;; ts-mode turns on without its grammar.
(setq treesit-auto-install-grammar 'ask)

(defconst riven/treesit-grammar-langs
  '(bash c cpp cmake css elisp go html javascript json make ocaml
    python php typescript tsx ruby rust sql vue yaml toml zig haskell
    kotlin java lua dockerfile elixir heex angular)
  "Languages whose tree-sitter grammars RivenEmacs manages.")

(defun install-treesit-language-grammars ()
  "Install tree-sitter language grammars.
Install every grammar in `riven/treesit-grammar-langs' that is not yet
available. Safe to re-run."
  (interactive)
  (dolist (lang riven/treesit-grammar-langs)
    (unless (treesit-language-available-p lang)
      (condition-case err
          (treesit-install-language-grammar lang)
        (error (message "Failed to install tree-sitter grammar for %s: %s" lang err))))))

(defalias 'auto-install-treesit-grammars #'install-treesit-language-grammars
  "Backwards-compatible alias for `install-treesit-language-grammars'.")

(defun treesit-needs-install-p ()
  "Return non-nil if any managed tree-sitter grammar is not yet installed."
  (cl-some (lambda (lang) (not (treesit-language-available-p lang)))
           riven/treesit-grammar-langs))

;; Optional: Install tree-sitter grammars on first startup.
;; You can manually run M-x install-treesit-language-grammars to install,
;; or set rivenEmacs-auto-install-treesit to t in customize.
(when rivenEmacs-auto-install-treesit
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when (treesit-needs-install-p)
                (install-treesit-language-grammars)))))

(defun install-essential-treesit-grammars ()
  "Install essential tree-sitter grammars for common languages."
  (interactive)
  (dolist (lang '(bash c cpp css go html javascript json python rust typescript tsx yaml))
    (unless (treesit-language-available-p lang)
      (condition-case err
          (treesit-install-language-grammar lang)
        (error (message "Failed to install tree-sitter grammar for %s: %s" lang err))))))

(defcustom riven/treesit-auto-create-modes
  '(ielm-mode json-mode go-mode java-mode java-ts-mode
    php-mode php-ts-mode haskell-mode kotlin-mode swift-mode)
  "Major modes in which to eagerly create a tree-sitter parser.
Guarded so the `after-change-major-mode-hook' callback does nothing in the
many buffers (messages, fundamental, special, etc.) that will never use treesit."
  :type '(repeat symbol)
  :group 'rivenEmacs)

;; Auto-create tree-sitter parsers for supported modes
(defun +treesit-auto-create-parser ()
  "Auto-create tree-sitter parser for current mode.
No-op unless `major-mode' is in `riven/treesit-auto-create-modes'."
  (when (memq major-mode riven/treesit-auto-create-modes)
    (let ((lang (pcase major-mode
                  ('ielm-mode 'elisp)
                  ('json-mode 'json)
                  ('go-mode 'go)
                  ('java-mode 'java)
                  ('java-ts-mode 'java)
                  ('php-mode 'php)
                  ('php-ts-mode 'php)
                  ('haskell-mode 'haskell)
                  ('kotlin-mode 'kotlin)
                  ('swift-mode 'swift)
                  (_ nil))))
      (when (and lang (treesit-language-available-p lang))
        (condition-case err
            (treesit-parser-create lang)
          (error (message "Failed to create tree-sitter parser for %s: %s" lang err)))))))

;; Add hook to auto-create parsers
(add-hook 'after-change-major-mode-hook #'+treesit-auto-create-parser)

;; @Deprecated
;; (use-package combobulate
;;   :vc (:url "https://github.com/mickeynp/combobulate")
;;   :preface
;;   ;; You can customize Combobulate's key prefix here.
;;   ;; Note that you may have to restart Emacs for this to take effect!
;;   (setq combobulate-key-prefix "C-c C-o")

;;   ;; Optional, but recommended.
;;   ;;
;;   ;; You can manually enable Combobulate with `M-x
;;   ;; combobulate-mode'.
;;   :hook
;;   ((python-ts-mode . combobulate-mode)
;;    (js-ts-mode . combobulate-mode)
;;    (html-ts-mode . combobulate-mode)
;;    (css-ts-mode . combobulate-mode)
;;    (yaml-ts-mode . combobulate-mode)
;;    (typescript-ts-mode . combobulate-mode)
;;    (json-ts-mode . combobulate-mode)
;;    (tsx-ts-mode . combobulate-mode)))

(provide 'init-treesit)

;;; init-treesit.el ends here
