;; -*- coding: utf-8; lexical-binding: t -*-

;;; init-gpt.el --- gptel configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; GPT configuration: gptel core + optional extension commands.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'init-gpt-helper)

(declare-function gptel-make-deepseek "gptel-openai-extras" (&rest args))
(declare-function gptel-make-openai "gptel-openai" (name &rest args))
(declare-function gptel-make-openai-responses "gptel-openai-responses" (name &rest args))
(declare-function gptel-auto-scroll "gptel" (&rest args))
(declare-function gptel-end-of-response "gptel" (&rest args))
(declare-function gptel-api-key-from-auth-source "gptel" (&optional host user port))
(declare-function exec-path-from-shell-copy-envs "exec-path-from-shell" (variables))
(declare-function gptel-mcp-connect "gptel-integrations"
                  (&optional servers server-callback interactive))
(declare-function mcp-hub-start-all-server "mcp-hub" ())
(declare-function mcp--status "mcp" (connection))
(declare-function gptel-extensions-refactor "gptel-extensions" (&optional arg))
(declare-function gptel-extensions-ask-document "gptel-extensions" (&optional arg))
(defvar gptel-model)
(defvar gptel-backend)
(defvar gptel-api-key)
(defvar gptel--known-tools)
(defvar mcp-hub-servers)
(defvar mcp-server-connections)

(defcustom rivenEmacs-gptel-backend 'zayfen
  "Preferred backend for gptel."
  :type '(choice (const :tag "Zayfen (OpenAI-compatible)" zayfen)
                 (const :tag "DeepSeek" deepseek)
                 (const :tag "Use gptel default backend" default))
  :group 'rivenEmacs)

(defcustom rivenEmacs-zayfen-host "api.zayfen.com"
  "Host (without protocol) for the Zayfen OpenAI-compatible endpoint."
  :type 'string
  :group 'rivenEmacs)

(defcustom rivenEmacs-zayfen-chat-models
  '(deepseek-v4-flash deepseek-v4-pro glm-5 glm-5-turbo)
  "Chat-Completions models exposed by the Zayfen backend.
These models are served via /v1/chat/completions."
  :type '(repeat symbol)
  :group 'rivenEmacs)

(defcustom rivenEmacs-zayfen-responses-models
  '((gpt-5.4 :capabilities (tool-use json responses-api))
    (gpt-5.5 :capabilities (tool-use json responses-api)))
  "Responses-API models exposed by the Zayfen backend.
These models are served via /v1/responses."
  :type '(repeat (choice symbol (cons symbol plist)))
  :group 'rivenEmacs)

(defcustom rivenEmacs-zayfen-default-model 'deepseek-v4-pro
  "Default model used with the Zayfen Chat-Completions backend."
  :type 'symbol
  :group 'rivenEmacs)

(defcustom rivenEmacs-gptel-mcp-auto-connect nil
  "Whether to connect MCP tools to gptel automatically after init."
  :type 'boolean
  :group 'rivenEmacs)

(defcustom rivenEmacs-gptel-chatgpt-oauth-backend nil
  "Optional ChatGPT/Codex OAuth backend name for gptel.
When non-nil, register a `gptel-make-openai-oauth' backend with this name
on gptel load; run `M-x gptel-openai-oauth-login' once to authenticate.
The `C-c a W' Work preset uses this backend when it is available."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'rivenEmacs)

(defcustom rivenEmacs-gptel-chatgpt-oauth-model 'gpt-5.3-codex
  "Model used by the Work preset when the ChatGPT OAuth backend is active.
The ChatGPT subscription backend exposes models such as `gpt-5.3-codex'
and `gpt-5.4'; adjust this if your account supports a different one."
  :type 'symbol
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

(defcustom rivenEmacs-mcp-enable-browser-server t
  "Whether to enable the Playwright browser MCP server."
  :type 'boolean
  :group 'rivenEmacs)

(defcustom rivenEmacs-mcp-enable-browser-use-server t
  "Whether to enable the browser-use MCP server."
  :type 'boolean
  :group 'rivenEmacs)

(defcustom rivenEmacs-mcp-enable-brave-search-server t
  "Whether to enable the Brave Search MCP server."
  :type 'boolean
  :group 'rivenEmacs)

(defcustom rivenEmacs-mcp-enable-tavily-server t
  "Whether to enable the Tavily MCP server."
  :type 'boolean
  :group 'rivenEmacs)

(defcustom rivenEmacs-mcp-brave-api-key-env "BRAVE_API_KEY"
  "Environment variable name containing Brave Search API key."
  :type 'string
  :group 'rivenEmacs)

(defcustom rivenEmacs-mcp-tavily-api-key-env "TAVILY_API_KEY"
  "Environment variable name containing Tavily API key."
  :type 'string
  :group 'rivenEmacs)

(defcustom rivenEmacs-mcp-browser-headless t
  "Whether to run Playwright MCP in headless mode."
  :type 'boolean
  :group 'rivenEmacs)

(defcustom rivenEmacs-mcp-memory-file
  (expand-file-name "mcp-memory.jsonl" user-emacs-directory)
  "Storage file used by the MCP memory server."
  :type 'file
  :group 'rivenEmacs)

(defcustom rivenEmacs-gptel-shell-env-fallback t
  "Whether to read missing API keys from an interactive login shell."
  :type 'boolean
  :group 'rivenEmacs)

(defconst riven/gptel-api-key-env-vars
  '("ZAYFEN_API_KEY" "DEEPSEEK_API_KEY" "OPENAI_API_KEY"
    "ANTHROPIC_AUTH_TOKEN" "TAVILY_API_KEY" "BRAVE_API_KEY" "GITHUB_TOKEN")
  "Environment variable names used for AI and MCP credentials.")

(defun riven/gptel--sync-api-key-env-vars ()
  "Sync AI API key env vars from shell when possible."
  (when (require 'exec-path-from-shell nil t)
    (when (fboundp 'exec-path-from-shell-copy-envs)
      (exec-path-from-shell-copy-envs riven/gptel-api-key-env-vars))))

(defun riven/gptel--read-env (name)
  "Return env var NAME value when non-empty, else nil."
  (let ((value (getenv name)))
    (cond
     ((not (string-empty-p (or value "")))
      value)
     ((and rivenEmacs-gptel-shell-env-fallback
           (stringp shell-file-name)
           (file-executable-p shell-file-name)
           (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\'" name))
      (with-temp-buffer
        (let ((status (call-process shell-file-name nil t nil
                                    "-l" "-i" "-c"
                                    (format "printenv %s" name))))
          (when (and (integerp status) (zerop status))
            (let ((shell-value (string-trim-right (buffer-string))))
              (unless (string-empty-p shell-value)
                (setenv name shell-value)
                shell-value))))))
     (t nil))))

(defun riven/gptel-configure-api-key ()
  "Configure API key for gptel default backends."
  (riven/gptel--sync-api-key-env-vars)
  (let ((openai-key (riven/gptel--read-env "OPENAI_API_KEY")))
    (cond
     ((not (string-empty-p (or openai-key "")))
      (setq gptel-api-key openai-key))
     ((ignore-errors
        (not (string-empty-p (or (gptel-api-key-from-auth-source) ""))))
      nil)
     (t
      (message "[gptel] Missing API key: set OPENAI_API_KEY (or auth-source entry gptel-api-key).")))))

(defun riven/gptel--zayfen-api-key ()
  "Resolve the Zayfen API key from env or auth-source."
  (or (riven/gptel--read-env "ZAYFEN_API_KEY")
      (ignore-errors
        (gptel-api-key-from-auth-source rivenEmacs-zayfen-host "apikey"))
      (ignore-errors
        (gptel-api-key-from-auth-source rivenEmacs-zayfen-host))))

(defun riven/gptel--configure-zayfen-backend ()
  "Configure the Zayfen OpenAI-compatible backends.

Registers two backends sharing the same host:
- \"Zayfen\"           via /v1/chat/completions (deepseek, glm, ...)
- \"Zayfen-Responses\" via /v1/responses        (gpt-5.x reasoning models)

The default `gptel-backend' is set to the Chat-Completions one.
Switch backend/model interactively via `gptel-menu'."
  (let ((key (riven/gptel--zayfen-api-key)))
    (cond
     ((not (require 'gptel nil t))
      (message "[gptel] gptel not available; cannot register Zayfen backend."))
     ((not (fboundp 'gptel-make-openai))
      (message "[gptel] gptel-make-openai unavailable; keeping default backend."))
     ((string-empty-p (or key ""))
      (message "[gptel] ZAYFEN_API_KEY is empty; using default backend."))
     (t
      (let ((chat-backend
             (gptel-make-openai "Zayfen"
               :host rivenEmacs-zayfen-host
               :protocol "https"
               :endpoint "/v1/chat/completions"
               :stream t
               :key key
               :models rivenEmacs-zayfen-chat-models)))
        (when (and rivenEmacs-zayfen-responses-models
                   (require 'gptel-openai-responses nil t)
                   (fboundp 'gptel-make-openai-responses))
          (gptel-make-openai-responses "Zayfen-Responses"
            :host rivenEmacs-zayfen-host
            :protocol "https"
            :endpoint "/v1/responses"
            :stream t
            :key key
            :models rivenEmacs-zayfen-responses-models))
        (setq gptel-model rivenEmacs-zayfen-default-model
              gptel-backend chat-backend))))))

(defun riven/gptel--configure-deepseek-backend ()
  "Configure the official DeepSeek backend."
  (let ((deepseek-key
         (or (riven/gptel--read-env "DEEPSEEK_API_KEY")
             (ignore-errors (gptel-api-key-from-auth-source "api.deepseek.com" "apikey"))
             (ignore-errors (gptel-api-key-from-auth-source "api.deepseek.com")))))
    (cond
     ((not (require 'gptel-openai-extras nil t))
      (message "[gptel] gptel-openai-extras not found; keeping default backend."))
     ((not (fboundp 'gptel-make-deepseek))
      (message "[gptel] gptel-make-deepseek unavailable; keeping default backend."))
     ((string-empty-p (or deepseek-key ""))
      (message "[gptel] DEEPSEEK_API_KEY is empty; using default backend."))
     (t
      (setq gptel-model 'deepseek-chat
            gptel-backend
            (gptel-make-deepseek "DeepSeek" :stream t :key deepseek-key))))))

(defun riven/gptel-configure-backend ()
  "Set up gptel backend with graceful fallback."
  (riven/gptel--sync-api-key-env-vars)
  (pcase rivenEmacs-gptel-backend
    ('zayfen   (riven/gptel--configure-zayfen-backend))
    ('deepseek (riven/gptel--configure-deepseek-backend))
    (_ nil)))

(defun riven/gptel-configure-chatgpt-oauth ()
  "Register the optional ChatGPT subscription (Codex) backend."
  (when rivenEmacs-gptel-chatgpt-oauth-backend
    (require 'gptel-openai-oauth nil t)
    (if (fboundp 'gptel-make-openai-oauth)
        (gptel-make-openai-oauth rivenEmacs-gptel-chatgpt-oauth-backend)
      (message "[gptel] gptel-make-openai-oauth is unavailable"))))

(defun riven/gptel-work ()
  "Open a gptel chat with the ChatGPT Work-style agent preset.
Applies the `work' preset buffer-locally: core filesystem/grep/shell/web
tools, an agent system prompt, and confirmation for destructive tools."
  (interactive)
  (require 'gptel)
  (require 'gptel-transient)
  (let* ((backend (default-value 'gptel-backend))
         (buffer (gptel (format "*%s-Work*"
                                (if backend
                                    (gptel-backend-name backend)
                                  "gptel")))))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (gptel-preset 'work
                    (lambda (sym val)
                      (set (make-local-variable sym) val)))
      (when-let* ((oauth-name rivenEmacs-gptel-chatgpt-oauth-backend)
                  (oauth-backend
                   (ignore-errors (gptel-get-backend oauth-name))))
        (setq-local gptel-backend oauth-backend
                    gptel-model rivenEmacs-gptel-chatgpt-oauth-model))
      (message "gptel Work mode: core tools + confirmation; model=%s backend=%s"
               gptel-model (gptel-backend-name gptel-backend)))
    buffer))

(defun riven/gptel--mcp-command-spec (binary npm-package)
  "Build MCP server command plist from BINARY or NPM-PACKAGE fallback."
  (if-let* ((bin (executable-find binary)))
      `(:command ,bin)
    `(:command "npx"
      :args ("-y" ,npm-package)
      :env (:NPM_CONFIG_REGISTRY "https://registry.npmjs.org"))))

(defun riven/gptel--mcp-browser-server-spec ()
  "Return Playwright browser MCP server config."
  (let ((extra-args (append (when rivenEmacs-mcp-browser-headless '("--headless"))
                            '("--isolated" "--output-mode" "stdout"))))
    (if-let* ((bin (executable-find "playwright-mcp")))
        `("browser" . (:command ,bin :args ,extra-args))
      `("browser" . (:command "npx"
                     :args ,(append '("-y" "@playwright/mcp") extra-args)
                     :env (:NPM_CONFIG_REGISTRY "https://registry.npmjs.org"))))))

(defun riven/gptel--mcp-browser-use-server-spec ()
  "Return browser-use MCP server config."
  (if-let* ((uvx-bin (executable-find "uvx")))
      `("browser-use" . (:command ,uvx-bin :args ("browser-use" "--mcp")))
    (if-let* ((browser-use-bin (executable-find "browser-use")))
        `("browser-use" . (:command ,browser-use-bin :args ("--mcp")))
      `("browser-use" . (:command "uvx" :args ("browser-use" "--mcp"))))))

(defun riven/gptel--mcp-brave-search-server-spec ()
  "Return Brave Search MCP server config, or nil when API key is missing."
  (let ((api-key (getenv rivenEmacs-mcp-brave-api-key-env)))
    (unless (string-empty-p (or api-key ""))
      (if-let* ((bin (executable-find "mcp-server-brave-search")))
          `("brave-search" . (:command ,bin :env (:BRAVE_API_KEY ,api-key)))
        `("brave-search" . (:command "npx"
                            :args ("-y" "@modelcontextprotocol/server-brave-search")
                            :env (:NPM_CONFIG_REGISTRY "https://registry.npmjs.org"
                                  :BRAVE_API_KEY ,api-key)))))))

(defun riven/gptel--mcp-tavily-server-spec ()
  "Return Tavily MCP server config, or nil when API key is missing."
  (let ((api-key (getenv rivenEmacs-mcp-tavily-api-key-env)))
    (unless (string-empty-p (or api-key ""))
      (if-let* ((bin (executable-find "tavily-mcp")))
          `("tavily" . (:command ,bin :env (:TAVILY_API_KEY ,api-key)))
        `("tavily" . (:command "npx"
                      :args ("-y" "tavily-mcp")
                      :env (:NPM_CONFIG_REGISTRY "https://registry.npmjs.org"
                            :TAVILY_API_KEY ,api-key)))))))

(defun riven/gptel-mcp-popular-servers ()
  "Return popular MCP servers config for mcp.el."
  (unless (file-exists-p rivenEmacs-mcp-memory-file)
    (make-directory (file-name-directory rivenEmacs-mcp-memory-file) t)
    (write-region "" nil rivenEmacs-mcp-memory-file nil 'silent))
  (set-file-modes rivenEmacs-mcp-memory-file #o600)
  (let ((servers
         `(("filesystem" .
            ,(let ((spec (riven/gptel--mcp-command-spec
                          "mcp-server-filesystem"
                          "@modelcontextprotocol/server-filesystem")))
               (if (executable-find "mcp-server-filesystem")
                   (append spec `(:args ,rivenEmacs-mcp-filesystem-roots))
                 (plist-put spec :args
                            (append '("-y" "@modelcontextprotocol/server-filesystem")
                                    rivenEmacs-mcp-filesystem-roots)))))
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
    (when rivenEmacs-mcp-enable-browser-server
      (setq servers
            (append servers (list (riven/gptel--mcp-browser-server-spec)))))
    (when rivenEmacs-mcp-enable-browser-use-server
      (setq servers
            (append servers (list (riven/gptel--mcp-browser-use-server-spec)))))
    (when rivenEmacs-mcp-enable-brave-search-server
      (if-let* ((brave-spec (riven/gptel--mcp-brave-search-server-spec)))
          (setq servers (append servers (list brave-spec)))
        (message "[gptel-mcp] %s is missing; skipping brave-search server"
                 rivenEmacs-mcp-brave-api-key-env)))
    (when rivenEmacs-mcp-enable-tavily-server
      (if-let* ((tavily-spec (riven/gptel--mcp-tavily-server-spec)))
          (setq servers (append servers (list tavily-spec)))
        (message "[gptel-mcp] %s is missing; skipping tavily server"
                 rivenEmacs-mcp-tavily-api-key-env)))
    servers))

(defun riven/gptel--mcp-all-connected-p ()
  "Return non-nil when all configured MCP servers are connected."
  (and (boundp 'mcp-server-connections)
       (boundp 'mcp-hub-servers)
       (cl-every (lambda (server)
                   (when-let* ((conn (gethash (car server) mcp-server-connections)))
                     (equal (mcp--status conn) 'connected)))
                 mcp-hub-servers)))

(defun riven/gptel--mcp-wait-and-register (deadline)
  "Poll until servers are ready (up to DEADLINE), then register tools."
  (cond
   ((riven/gptel--mcp-all-connected-p)
    (gptel-mcp-connect)
    (message "[gptel-mcp] Tools registered from %d MCP servers."
             (length mcp-hub-servers)))
   ((time-less-p deadline (current-time))
    (message "[gptel-mcp] Timed out waiting for MCP servers; registering available tools.")
    (gptel-mcp-connect))
   (t
    (run-with-timer 1 nil #'riven/gptel--mcp-wait-and-register deadline))))

(defun riven/gptel-mcp-connect-popular ()
  "Start MCP servers and register their tools into gptel."
  (interactive)
  (unless (require 'mcp-hub nil t)
    (user-error "mcp.el is unavailable"))
  (unless (require 'gptel-integrations nil t)
    (user-error "gptel integrations are unavailable"))
  (mcp-hub-start-all-server)
  (let ((deadline (time-add (current-time) 30)))
    (riven/gptel--mcp-wait-and-register deadline)))

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
  (riven/gptel-configure-api-key)
  (riven/gptel-configure-backend)
  (riven/gptel-configure-chatgpt-oauth)
  (require 'init-gpt-tools)
  (require 'gptel-integrations nil t))

;; MCP integration using lizqwerscott/mcp.el
;; Repository: https://github.com/lizqwerscott/mcp.el
(use-package mcp
  :vc (:url "https://github.com/lizqwerscott/mcp.el" :branch "master")
  :after gptel
  :commands (mcp-hub mcp-hub-start-all-server)
  :custom
  (mcp-hub-servers (riven/gptel-mcp-popular-servers))
  :config
  (require 'mcp-hub)
  ;; Compatibility shim: gptel-integrations calls (fn callback server-names syncp),
  ;; but mcp.el's mcp-hub-start-all-server accepts no args.  We must still invoke
  ;; the callback so gptel can register tools after servers start.
  (advice-add 'mcp-hub-start-all-server :around
              (lambda (orig-fn &optional callback &rest _ignored)
                (funcall orig-fn)
                (when (functionp callback)
                  (run-with-timer 1 nil callback))))
  (when rivenEmacs-gptel-mcp-auto-connect
    (add-hook 'after-init-hook #'riven/gptel-mcp-connect-popular)))


(use-package gpt-extensions
  :vc (:url "https://github.com/kamushadenes/gptel-extensions.el")
  :defer t
  :init
  ;; Package name is gpt-extensions, but command library is gptel-extensions.el.
  (autoload 'gptel-extensions-refactor "gptel-extensions" nil t)
  (autoload 'gptel-extensions-ask-document "gptel-extensions" nil t)
  ;; NOTE: gpt-extensions does not define gptel-rewrite-article,
  ;; gptel-summarize-document, gptel-query-devdoc, gptel-generate-commit-message
  ;; or gptel-translate-region.  Fallback commands for these names live in
  ;; init-gpt-helper.el; autoloading non-existent functions here would overwrite
  ;; those fallbacks and fail at call time.
  ;; `gptel-extensions-refactor' is bound in `keybindings-spec-ai.el'
  ;; (C-c a R), not on the standard `C-x =' key.
  )

(provide 'init-gpt)

;;; init-gpt.el ends here
