;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'init-config)

(defun smarter-yas-expand-next-field ()
  "Try `yas-expand`, then move to next field if no expansion happened."
  (interactive)
  (let ((old-point (point))
        (old-tick (buffer-chars-modified-tick)))
    (yas-expand)
    (when (and (eq old-point (point))
               (eq old-tick (buffer-chars-modified-tick)))
      (ignore-errors (yas-next-field)))))

(defcustom rivenEmacs-eglot-auto-install t
  "If non-nil, offer to install missing language servers on connect.
When a server binary is not on PATH, Eglot prompts in the minibuffer
with the exact install command; answering `y' runs it.  Set to nil
to disable all install prompts (servers must be installed manually)."
  :type 'boolean
  :group 'rivenEmacs)

(defun riven/eglot--ensure-server (binary install-cmd)
  "Ensure BINARY is on PATH, optionally installing via INSTALL-CMD.
INSTALL-CMD is a list like (\"npm\" \"i\" \"-g\" \"foo\") or
(\"pip\" \"install\" \"foo\"); any installer works since it is run
via `call-process' (no shell).  If BINARY is already on PATH, return
t immediately.  Otherwise, when `rivenEmacs-eglot-auto-install' is
non-nil, prompt in the minibuffer; on `y' run INSTALL-CMD
synchronously and surface its output.  Returns non-nil if BINARY is
available afterwards, nil if the user declined, install failed, or
auto-install is disabled.  If INSTALL-CMD is nil, only detect."
  (cond
   ((executable-find binary) t)
   ((or (null install-cmd) (not rivenEmacs-eglot-auto-install)) nil)
   ((not (y-or-n-p (format "Eglot: `%s' not found. Install (%s)? "
                           binary (mapconcat #'identity install-cmd " "))))
    (message "Install skipped.") nil)
   (t
    (message "Running `%s'..." (mapconcat #'identity install-cmd " "))
    (let* ((program (executable-find (car install-cmd)))
           (buf (get-buffer-create "*eglot server install*")))
      (if (not program)
          (message "Install FAILED: `%s' is not on PATH; install it manually."
                   (car install-cmd))
        (with-current-buffer buf
          (erase-buffer))
        (let ((status (condition-case err
                          (apply #'call-process program nil (list buf t) nil
                                 (cdr install-cmd))
                        (error
                         (message "Install FAILED: %s"
                                  (error-message-string err))
                         nil))))
          (if (eq status 0)
              (message "Installed %s." binary)
            (display-buffer buf)
            (message "Install FAILED (exit %s) — see %s."
                     status (buffer-name buf)))))
      (executable-find binary)))))

(defun riven/eglot--pick-server (candidates)
  "Return (CONTACT . INSTALL-CMD) for the first available server.
CANDIDATES is a list of (BINARY CONTACT INSTALL-CMD) triples, tried
in order.  For each, ensures BINARY is available (offering to install
via `riven/eglot--ensure-server' if missing); returns CONTACT of the
first one that ends up available, or nil."
  (cl-block nil
    (cl-loop for (bin contact install) in candidates
             when (riven/eglot--ensure-server bin install)
             do (cl-return contact))))

(defun riven/eglot-python-contact (&optional _interactive _project)
  "Return preferred Eglot server contact for Python, or nil.
Offers to install the chosen server if missing.  Tries basedpyright,
then pyright, then ruff, finally pylsp.  Returns nil when no server is
installed and the user declines installation; Eglot then reports that
it cannot guess a server instead of silently trying a missing binary.
Optional arguments are accepted (and ignored) to satisfy eglot's
function-form contact contract (see `eglot-server-programs')."
  (riven/eglot--pick-server
   '((basedpyright-langserver ("basedpyright-langserver" "--stdio")
                               ("pip" "install" "basedpyright"))
     (pyright-langserver ("pyright-langserver" "--stdio")
                         ("npm" "i" "-g" "pyright"))
     (ruff ("ruff" "server") ("pip" "install" "ruff"))
     (pylsp ("pylsp") ("pip" "install" "python-lsp-server")))))

(defun riven/eglot-configure-python-server ()
  "Register the Python LSP for `python-mode' and `python-ts-mode'.
The contact is a function so servers are detected/offered for
installation when a Python buffer first connects."
  (when (boundp 'eglot-server-programs)
    (setf (alist-get 'python-mode eglot-server-programs nil nil #'eq)
          #'riven/eglot-python-contact)
    (setf (alist-get 'python-ts-mode eglot-server-programs nil nil #'eq)
          #'riven/eglot-python-contact)))

(defun riven/eglot--vue-tsdk ()
  "Return the TypeScript lib directory for Volar, or nil if none.
Looks in the current project first, then nvm-managed and globally
installed TypeScript trees."
  (or (when-let* ((proj (and (fboundp 'project-current) (project-current)))
                  (root (project-root proj))
                  (path (expand-file-name "node_modules/typescript/lib" root)))
        (and (file-directory-p path) path))
      ;; TypeScript installed via nvm.
      (car (sort (file-expand-wildcards
                  (expand-file-name "~/.nvm/versions/node/*/lib/node_modules/typescript/lib"))
                 #'string>))
      ;; TypeScript installed globally by the active npm.
      (when-let* ((npm-root (condition-case nil
                                (string-trim
                                 (shell-command-to-string "npm root -g"))
                              (error nil)))
                  (ts-lib (expand-file-name "typescript/lib" npm-root)))
        (and (file-directory-p ts-lib) ts-lib))
      ;; Homebrew / system global node_modules.
      (car (sort (file-expand-wildcards
                  (expand-file-name "/usr/local/lib/node_modules/typescript/lib"))
                 #'string>))
      (car (sort (file-expand-wildcards
                  (expand-file-name "/opt/homebrew/lib/node_modules/typescript/lib"))
                 #'string>))))

(defun riven/eglot--vue-contact (&optional _interactive _project)
  "Return the Eglot contact for vue-language-server (Volar).
Offers to install `@vue/language-server' (and `typescript' if the
TS SDK can't be located).  Returns a contact list, or signals an
error if the user declines or installation fails.  Optional
arguments are accepted (and ignored) to satisfy eglot's
function-form contact contract."
  (unless (executable-find "vue-language-server")
    (unless (riven/eglot--ensure-server
             "vue-language-server"
             '("npm" "i" "-g" "@vue/language-server" "@vue/language-core"))
      (error "vue-language-server not installed")))
  (let ((tsdk (or (riven/eglot--vue-tsdk)
                  (when (riven/eglot--ensure-server "tsc" '("npm" "i" "-g" "typescript"))
                    (riven/eglot--vue-tsdk)))))
    (unless tsdk
      (error "Cannot locate TypeScript lib directory for Volar"))
    `("vue-language-server" "--stdio"
      :initializationOptions
      (:typescript (:tsdk ,tsdk)
       :vue (:hybridMode :json-false)))))

(defun riven/eglot-configure-vue-server ()
  "Register vue-language-server (Volar) for `vue-ts-mode' in Eglot.
The contact is a function, so missing packages are detected and
offered for installation the first time a Vue buffer connects,
not at startup."
  (when (boundp 'eglot-server-programs)
    (setf (alist-get 'vue-ts-mode eglot-server-programs nil nil #'eq)
          #'riven/eglot--vue-contact)))

(defun riven/eglot-kotlin-contact (&optional _interactive _project)
  "Return Eglot server contact for Kotlin, or nil if none can be set up.
Offers to install `kotlin-language-server' (npm) or `kotlin-lsp'
(brew) when missing.  Returns the contact of the first available
server, or nil if the user declines.  Optional arguments are
accepted (and ignored) to satisfy eglot's function-form contact
contract."
  (or (riven/eglot--pick-server
       '((kotlin-lsp ("kotlin-lsp" "--stdio")
                      ("brew" "install" "kotlin-language-server"))
         (kotlin-language-server ("kotlin-language-server")
                                  ("npm" "i" "-g" "kotlin-language-server"))))
      (error "No Kotlin language server available")))

(defun riven/eglot-configure-kotlin-server ()
  "Register the Kotlin LSP for `kotlin-mode' and `kotlin-ts-mode'.
The contact is a function so servers are detected/offered for
installation when a Kotlin buffer first connects."
  (when (boundp 'eglot-server-programs)
    (setf (alist-get 'kotlin-mode eglot-server-programs nil nil #'eq)
          #'riven/eglot-kotlin-contact)
    (setf (alist-get 'kotlin-ts-mode eglot-server-programs nil nil #'eq)
          #'riven/eglot-kotlin-contact)))

(defun riven/eglot-register-simple-server (modes binary contact install-cmd)
  "Register a function-form CONTACT for MODES using BINARY + INSTALL-CMD.
MODES is a list of major-mode symbols.  BINARY is the executable to
detect; CONTACT is the eglot contact list (e.g. (\"clangd\")); INSTALL-CMD
is a list run on user consent to install BINARY (e.g.
(\"brew\" \"install\" \"llvm\")).  If BINARY is missing at connect time
the user is offered INSTALL-CMD.  If INSTALL-CMD is nil the server is
only detected, not installed."
  (when (boundp 'eglot-server-programs)
    (let ((fn (lambda (&optional _interactive _project)
                (if (riven/eglot--ensure-server binary install-cmd)
                    contact
                  (error "`%s' not installed" binary)))))
      (dolist (mode modes)
        (setf (alist-get mode eglot-server-programs nil nil #'eq) fn)))))

(defun riven/eglot-configure-default-servers ()
  "Register common LSPs with on-demand install for built-in modes.
Each entry below overrides eglot's built-in contact for that mode
with a function-form one that prompts to install the server if it
is missing.  Modes not listed here keep eglot's defaults (no
auto-install)."
  (riven/eglot-register-simple-server
   '(rust-mode rust-ts-mode) "rust-analyzer" '("rust-analyzer")
   '("rustup" "component" "add" "rust-analyzer"))
  (riven/eglot-register-simple-server
   '(c-mode c-ts-mode c++-mode c++-ts-mode objc-mode) "clangd" '("clangd")
   '("brew" "install" "llvm"))
  (riven/eglot-register-simple-server
   '(java-mode java-ts-mode) "jdtls" '("jdtls")
   '("brew" "install" "jdtls"))
  (riven/eglot-register-simple-server
   '(php-mode php-ts-mode) "phpactor" '("phpactor" "language-server")
   '("composer" "global" "require" "phpactor/phpactor"))
  ;; JS/TS via typescript-language-server (covers js-ts-mode,
  ;; typescript-ts-mode, tsx-ts-mode, etc.).
  (riven/eglot-register-simple-server
   '(typescript-ts-mode tsx-ts-mode js-ts-mode) "typescript-language-server"
   '("typescript-language-server" "--stdio")
   '("npm" "i" "-g" "typescript-language-server" "typescript")))

(defun riven/eglot-client-capabilities-no-file-watch (orig-fn server)
  "Call ORIG-FN for SERVER, disabling dynamic file-watch registration."
  (let* ((caps (funcall orig-fn server))
         (workspace (plist-get caps :workspace))
         (watch (and (listp workspace)
                     (plist-get workspace :didChangeWatchedFiles))))
    (when (listp watch)
      (setq watch (plist-put watch :dynamicRegistration :json-false))
      (setq workspace (plist-put workspace :didChangeWatchedFiles watch))
      (setq caps (plist-put caps :workspace workspace)))
    caps))

(use-package yasnippet
  :ensure t
  :hook ((prog-mode LaTeX-mode org-mode markdown-mode) . yas-minor-mode-on)
  :bind
  (:map yas-keymap
        ("TAB" . smarter-yas-expand-next-field)
        ([tab] . smarter-yas-expand-next-field))
  :config
  ;; yasnippet's user option is `yas-snippet-dirs' (plural); there is no
  ;; `yas-snippet-dir' variable.
  (add-to-list 'yas-snippet-dirs rivenEmacs-snippets-dir))

(use-package yasnippet-snippets
  :ensure t)

(use-package eglot
  :ensure nil
  :init
  (with-eval-after-load 'eglot
    (unless (advice-member-p #'riven/eglot-client-capabilities-no-file-watch
                             #'eglot-client-capabilities)
      (advice-add #'eglot-client-capabilities :around
                  #'riven/eglot-client-capabilities-no-file-watch)))
  (dolist (mode rivenEmacs-lsp-modes)
    (add-hook (intern (format "%s-hook" mode)) #'eglot-ensure))
  :bind (:map eglot-mode-map
              ("M-?" . xref-find-references))
  :custom
  ;; Built-in ElDoc behavior is enough; keep it compact and predictable.
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-idle-delay 0.15)
  (eglot-sync-connect nil)
  (eglot-connect-timeout 30)
  (eglot-autoshutdown t)
  (eglot-report-progress nil)
  (eglot-send-changes-idle-time 0.2)
  (eglot-extend-to-xref t)
  ;; Avoid cursor-line height jitter from code-action overlays in margin/nearby.
  ;; Keep hints in mode-line only to preserve stable line metrics in all languages.
  (eglot-code-action-indications '(mode-line))
  ;; Use ASCII indicator to avoid emoji fallback fonts affecting glyph height.
  (eglot-code-action-indicator "*")
  (eglot-events-buffer-config '(:size 0 :format short))
  ;; on-type formatting makes a synchronous blocking request to the server,
  ;; causing Emacs to freeze on characters like `;' and RET.
  ;; (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
  :config
  (riven/eglot-configure-python-server)
  (riven/eglot-configure-vue-server)
  (riven/eglot-configure-kotlin-server)
  (riven/eglot-configure-default-servers)
  (setq-default jsonrpc-default-request-timeout 30)
  (keymap-global-set "M-." #'riven/xref-find-definitions-or-search)
  (keymap-global-set "M-," #'xref-go-back)
  (keymap-global-set "M-?" #'xref-find-references))

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :branch "main")
  :after eglot
  :config
  ;; Disable bytecode mode for eglot-booster to preserve proper UTF-8 encoding
  (setq eglot-booster-io-only t)
  (eglot-booster-mode))

(provide 'init-eglot)
;;; init-eglot.el ends here
