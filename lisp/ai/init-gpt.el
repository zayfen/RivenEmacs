;; -*- coding: utf-8; lexical-binding: t -*-

;;; init-gpt.el --- gptel configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; GPT configuration: gptel core + optional extension commands.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'init-prompt-template nil t)
(require 'init-gpt-helper)

(declare-function gptel-make-deepseek "gptel-openai-extras" (&rest args))
(declare-function gptel-auto-scroll "gptel" (&rest args))
(declare-function gptel-end-of-response "gptel" (&rest args))
(declare-function gptel-mcp-connect "gptel-integrations"
                  (&optional servers server-callback interactive))
(declare-function mcp-hub-start-all-server "mcp-hub" (&optional callback servers syncp))
(declare-function gptel-extensions-refactor "gptel-extensions" (&optional arg))
(declare-function gptel-extensions-ask-document "gptel-extensions" (&optional arg))
(declare-function gptel-rewrite-article "gptel-extensions" (&optional arg))
(declare-function gptel-summarize-document "gptel-extensions" (&optional arg))
(declare-function gptel-query-devdoc "gptel-extensions" (&optional arg))
(declare-function gptel-generate-commit-message "gptel-extensions" (&optional arg))
(declare-function gptel-translate-region "gptel-extensions" (&optional arg))
(defvar gptel-model)
(defvar gptel-backend)
(defvar gptel--known-tools)
(defvar mcp-hub-servers)

(defcustom rivenEmacs-gptel-backend 'deepseek
  "Preferred backend for gptel."
  :type '(choice (const :tag "DeepSeek" deepseek)
                 (const :tag "Use gptel default backend" default))
  :group 'rivenEmacs)

(defcustom rivenEmacs-gptel-mcp-auto-connect nil
  "Whether to connect MCP tools to gptel automatically after init."
  :type 'boolean
  :group 'rivenEmacs)

(defcustom rivenEmacs-mcp-filesystem-roots
  (list (or (getenv "DEFAULT_WORKSPACE")
            (expand-file-name "~/")))
  "Directory roots exposed to the MCP filesystem server."
  :type '(repeat directory)
  :group 'rivenEmacs)

(defcustom rivenEmacs-mcp-enable-github-server nil
  "Whether to enable GitHub MCP server when token is available."
  :type 'boolean
  :group 'rivenEmacs)

(defcustom rivenEmacs-mcp-memory-file
  (expand-file-name "mcp-memory.jsonl" user-emacs-directory)
  "Storage file used by the MCP memory server."
  :type 'file
  :group 'rivenEmacs)

(defun riven/gptel-configure-backend ()
  "Set up gptel backend with graceful fallback."
  (let ((deepseek-key (getenv "DEEPSEEK_API_KEY")))
    (cond
     ((not (eq rivenEmacs-gptel-backend 'deepseek))
      nil)
     ((not (require 'gptel-openai-extras nil t))
      (message "[gptel] gptel-openai-extras not found; keeping default backend."))
     ((not (fboundp 'gptel-make-deepseek))
      (message "[gptel] gptel-make-deepseek unavailable; keeping default backend."))
     ((string-empty-p (or deepseek-key ""))
      (message "[gptel] DEEPSEEK_API_KEY is empty; keeping default backend."))
     (t
      (setq gptel-model 'deepseek-chat
            gptel-backend
            (gptel-make-deepseek "DeepSeek" :stream t :key deepseek-key))))))

(defun riven/gptel--mcp-command-spec (binary npm-package)
  "Build MCP server command plist from BINARY or NPM-PACKAGE fallback."
  (if-let* ((bin (executable-find binary)))
      `(:command ,bin)
    `(:command "npx"
      :args ("-y" ,npm-package)
      :env (:NPM_CONFIG_REGISTRY "https://registry.npmjs.org"))))

(defun riven/gptel-mcp-popular-servers ()
  "Return popular MCP servers config for mcp.el."
  (unless (file-exists-p rivenEmacs-mcp-memory-file)
    (make-directory (file-name-directory rivenEmacs-mcp-memory-file) t)
    (write-region "" nil rivenEmacs-mcp-memory-file nil 'silent))
  (set-file-modes rivenEmacs-mcp-memory-file #o600)
  (let ((servers
         `(("filesystem" .
            ,(append (riven/gptel--mcp-command-spec
                      "mcp-server-filesystem"
                      "@modelcontextprotocol/server-filesystem")
                     `(:roots ,rivenEmacs-mcp-filesystem-roots)))
           ("memory" .
            ,(append (riven/gptel--mcp-command-spec
                      "mcp-server-memory"
                      "@modelcontextprotocol/server-memory")
                     `(:env (:MEMORY_FILE_PATH ,rivenEmacs-mcp-memory-file))))
           ("sequential-thinking" .
            ,(riven/gptel--mcp-command-spec
              "mcp-server-sequential-thinking"
              "@modelcontextprotocol/server-sequential-thinking"))
           ("everything" .
            ,(riven/gptel--mcp-command-spec
              "mcp-server-everything"
              "@modelcontextprotocol/server-everything")))))
    (when (and rivenEmacs-mcp-enable-github-server
               (or (getenv "GITHUB_PERSONAL_ACCESS_TOKEN")
                   (getenv "GITHUB_TOKEN")))
      (let ((github-token (or (getenv "GITHUB_PERSONAL_ACCESS_TOKEN")
                              (getenv "GITHUB_TOKEN"))))
        (setq servers
              (append servers
                      (list
                       `("github" .
                         ,(append (riven/gptel--mcp-command-spec
                                   "mcp-server-github"
                                   "@modelcontextprotocol/server-github")
                                  `(:env (:GITHUB_PERSONAL_ACCESS_TOKEN
                                          ,github-token)))))))))
    servers))

(defun riven/gptel-mcp-connect-popular ()
  "Connect configured MCP servers and register tools into gptel."
  (interactive)
  (unless (require 'mcp-hub nil t)
    (user-error "mcp.el is unavailable"))
  (unless (require 'gptel-integrations nil t)
    (user-error "gptel integrations are unavailable"))
  (gptel-mcp-connect (mapcar #'car mcp-hub-servers) 'sync))

(defun riven/gptel-mcp-verify ()
  "Verify MCP integration and return status plist."
  (interactive)
  (riven/gptel-mcp-connect-popular)
  (let* ((mcp-categories
          (cl-loop for (category . _tools) in gptel--known-tools
                   if (string-prefix-p "mcp-" category)
                   collect category))
         (status `(:servers ,(mapcar #'car mcp-hub-servers)
                   :tool-categories ,mcp-categories
                   :tools-count ,(length mcp-categories))))
    (if mcp-categories
        (message "[gptel-mcp] OK: %d tool categories registered: %s"
                 (length mcp-categories)
                 (mapconcat #'identity mcp-categories ", "))
      (user-error "[gptel-mcp] no MCP tools registered"))
    status))

(use-package gptel
  :vc (:url "https://github.com/karthink/gptel" :branch "master")
  :commands (gptel gptel-menu gptel-send gptel-rewrite)
  :config
  (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response)
  (riven/gptel-configure-backend)
  (require 'gptel-integrations nil t))

(use-package mcp
  :vc (:url "https://github.com/lizqwerscott/mcp.el" :branch "master")
  :after gptel
  :commands (mcp-hub mcp-hub-start-all-server)
  :custom
  (mcp-hub-servers (riven/gptel-mcp-popular-servers))
  :config
  (require 'mcp-hub)
  (when rivenEmacs-gptel-mcp-auto-connect
    (add-hook 'after-init-hook #'riven/gptel-mcp-connect-popular)))

(use-package gpt-extensions
  :vc (:url "https://github.com/kamushadenes/gptel-extensions.el")
  :defer t
  :init
  ;; Package name is gpt-extensions, but command library is gptel-extensions.el.
  (autoload 'gptel-extensions-refactor "gptel-extensions" nil t)
  (autoload 'gptel-extensions-ask-document "gptel-extensions" nil t)
  (autoload 'gptel-rewrite-article "gptel-extensions" nil t)
  (autoload 'gptel-summarize-document "gptel-extensions" nil t)
  (autoload 'gptel-query-devdoc "gptel-extensions" nil t)
  (autoload 'gptel-generate-commit-message "gptel-extensions" nil t)
  (autoload 'gptel-translate-region "gptel-extensions" nil t)
  (keymap-global-set "C-x =" #'gptel-extensions-refactor))

(provide 'init-gpt)

;;; init-gpt.el ends here
