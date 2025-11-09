;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-treesit.el --- Config for treesit

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
        (angular . ("https://github.com/dlvandenberg/tree-sitter-angular"))))

(defun install-treesit-language-grammars ()
  "Install tree-sitter language grammars."
  (interactive)
  (dolist (lang '(bash c cpp cmake css elisp go html javascript json make ocaml python php typescript tsx ruby rust sql vue yaml toml zig haskell kotlin java lua dockerfile elixir heex angular))
    (unless (treesit-language-available-p lang)
      (condition-case err
          (treesit-install-language-grammar lang)
        (error (message "Failed to install tree-sitter grammar for %s: %s" lang err))))))

(defun auto-install-treesit-grammars ()
  "Auto-install tree-sitter grammars for better syntax highlighting."
  (interactive)
  (dolist (lang '(bash c cpp cmake css elisp go html javascript json make ocaml python php typescript tsx ruby rust sql vue yaml toml zig haskell kotlin java lua dockerfile elixir heex angular))
    (unless (treesit-language-available-p lang)
      (condition-case err
          (treesit-install-language-grammar lang)
        (error (message "Failed to install tree-sitter grammar for %s: %s" lang err))))))

(defun treesit-needs-install-p ()
  "Check if any tree-sitter grammars need to be installed."
  (let ((langs-to-check '(bash c cpp cmake css elisp go html javascript json make ocaml python php typescript tsx ruby rust sql vue yaml toml zig haskell kotlin java lua dockerfile elixir heex angular)))
    (cl-some (lambda (lang) (not (treesit-language-available-p lang))) langs-to-check)))

;; Optional: Install tree-sitter grammars on first startup
;; You can manually run M-x auto-install-treesit-grammars to install
;; Or set rivenEmacs-auto-install-treesit to t in customize
(when rivenEmacs-auto-install-treesit
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when (treesit-needs-install-p)
                (auto-install-treesit-grammars)))))

;; Install essential grammars only on demand
(defun install-essential-treesit-grammars ()
  "Install essential tree-sitter grammars for common languages."
  (interactive)
  (dolist (lang '(bash c cpp css go html javascript json python rust typescript tsx yaml))
    (unless (treesit-language-available-p lang)
      (condition-case err
          (treesit-install-language-grammar lang)
        (error (message "Failed to install tree-sitter grammar for %s: %s" lang err))))))

(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (css-mode . css-ts-mode)
        (java-mode . java-ts-mode)
        (javascript-mode . js-ts-mode)
        (js-mode . js-ts-mode)
        (js-json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (sh-mode . bash-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (php-mode . php-ts-mode)
        (html-mode . html-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (dockerfile-mode . dockerfile-ts-mode)
        (toml-mode . toml-ts-mode)
        (rust-mode . rust-ts-mode)
        (ruby-mode . ruby-ts-mode)
        (cmake-mode . cmake-ts-mode)
        (haskell-mode . haskell-ts-mode)
        (kotlin-mode . kotlin-ts-mode)
        (lua-mode . lua-ts-mode)
        (make-mode . makefile-ts-mode)
        (conf-toml-mode . toml-ts-mode)
        (elixir-mode . elixir-ts-mode)
        (heex-mode . heex-ts-mode)
        (angular-mode . angular-ts-mode)))

;; Auto-create tree-sitter parsers for supported modes
(defun +treesit-auto-create-parser ()
  "Auto-create tree-sitter parser for current mode."
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
                (_ nil))))
    (when (and lang (treesit-language-available-p lang))
      (condition-case err
          (treesit-parser-create lang)
        (error (message "Failed to create tree-sitter parser for %s: %s" lang err))))))

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
