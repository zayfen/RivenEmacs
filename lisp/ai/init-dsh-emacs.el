;;; init-dsh-emacs.el --- DeepSeek Harness client -*- lexical-binding: t; -*-

;;; Commentary:
;; `dsh-emacs' is the Emacs client for DeepSeek Harness (`dsh web'): a
;; session list grouped by workspace, streaming chat with foldable tool calls
;; and reasoning, model/effort selection, attachments, and subagents.
;;
;; The package stays lazy and loads from its autoload on the first `C-c a d'
;; press.  With the default `dsh-emacs-base-url' it manages a loopback
;; server: when nothing answers at http://127.0.0.1:3080 it runs
;; `dsh web --host 127.0.0.1 --port 3080 --no-open' in a `*dsh-server*'
;; buffer and captures the launch token that server prints.
;;
;; To reuse a server started outside Emacs (for example the `dsh web' already
;; serving the browser UI), set `dsh-emacs-base-url' to the URL that server
;; printed -- including the `?token=...' query -- and set
;; `dsh-emacs-server-auto-start' to nil.  See `M-x customize-group RET
;; dsh-emacs RET'.
;;
;; The `dsh' CLI is already on `exec-path'; `dsh-emacs' only offers to
;; install it when the binary is missing.

;;; Code:

(use-package dsh-emacs
  :vc (:url "https://github.com/vritser/dsh-emacs")
  :commands (dsh-emacs dsh-emacs-new-session dsh-emacs-open-web
             dsh-emacs-health dsh-emacs-list-subagents))

(provide 'init-dsh-emacs)

;;; init-dsh-emacs.el ends here
