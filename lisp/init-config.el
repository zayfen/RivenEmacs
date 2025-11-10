;;; init-config.el --- Configuration management module -*- lexical-binding: t; -*-

;;; Commentary:
;; Centralized configuration management for RivenEmacs
;; This module provides customizable variables for common configurations

;;; Code:

(defgroup rivenEmacs nil
  "RivenEmacs configuration group."
  :group 'emacs)

(defcustom rivenEmacs-default-workspace
  (expand-file-name "~/")
  "Default workspace directory for RivenEmacs."
  :type 'directory
  :group 'rivenEmacs)

(defcustom rivenEmacs-proxy-http
  "127.0.0.1:7890"
  "HTTP proxy server address."
  :type 'string
  :group 'rivenEmacs)

(defcustom rivenEmacs-proxy-https
  "127.0.0.1:7890"
  "HTTPS proxy server address."
  :type 'string
  :group 'rivenEmacs)

(defcustom rivenEmacs-use-proxy
  nil
  "Whether to use proxy for network connections."
  :type 'boolean
  :group 'rivenEmacs)

(defcustom rivenEmacs-snippets-dir
  (expand-file-name "snippets" user-emacs-directory)
  "Directory for yasnippet snippets."
  :type 'directory
  :group 'rivenEmacs)

(defcustom rivenEmacs-org-directory
  (expand-file-name "~/org")
  "Directory for org-mode files."
  :type 'directory
  :group 'rivenEmacs)

(defcustom rivenEmacs-dashboard-delay
  0.5
  "Delay in seconds before opening dashboard."
  :type 'number
  :group 'rivenEmacs)

(defcustom rivenEmacs-lsp-modes
  '(python-ts-mode python-mode
    js-ts-mode javascript-mode
    typescript-ts-mode typescript-mode
    tsx-ts-mode
    rust-ts-mode rust-mode
    c-ts-mode c-mode
    c++-ts-mode c++-mode
    java-ts-mode java-mode
    kotlin-ts-mode kotlin-mode
    php-ts-mode php-mode
    web-mode vue-mode
    css-ts-mode css-mode
    html-ts-mode html-mode)
  "List of modes where LSP should be enabled."
  :type '(repeat symbol)
  :group 'rivenEmacs)

(defcustom rivenEmacs-auto-install-treesit
  nil
  "Whether to automatically install tree-sitter grammars on startup."
  :type 'boolean
  :group 'rivenEmacs)

(defun rivenEmacs-get-default-directory ()
  "Get the default workspace directory."
  (or (getenv "DEFAULT_WORKSPACE") 
      rivenEmacs-default-workspace))

(defun rivenEmacs-setup-proxy ()
  "Setup proxy based on configuration."
  (when rivenEmacs-use-proxy
    (let ((http-proxy (or (getenv "HTTP_PROXY") (getenv "http_proxy") rivenEmacs-proxy-http))
          (https-proxy (or (getenv "HTTPS_PROXY") (getenv "https_proxy") rivenEmacs-proxy-https)))
      (setq url-proxy-services
            `(("http" . ,http-proxy)
              ("https" . ,https-proxy))))))

(defun rivenEmacs-get-org-directory ()
  "Get the org directory, creating it if it doesn't exist."
  (let ((org-dir rivenEmacs-org-directory))
    (unless (file-directory-p org-dir)
      (make-directory org-dir t))
    org-dir))

(provide 'init-config)
;;; init-config.el ends here 