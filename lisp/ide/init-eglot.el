;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'init-config)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function project-root "project" (project))
(declare-function eglot-server-capable "eglot" (capability &optional server))
(declare-function eglot-current-server "eglot" ())
(declare-function eglot-completion-at-point "eglot" ())
(declare-function eglot-hover-eldoc-function "eglot" (cb &rest ignored))
(declare-function eglot--request "eglot" (server method params &rest args))
(declare-function eglot--TextDocumentIdentifier "eglot" ())
(declare-function eglot--pos-to-lsp-position "eglot" (&optional pos))
(declare-function eglot--sig-info "eglot" (sig &optional sig-active briefp))
(defvar eglot-mode-map)
(declare-function jsonrpc--process "jsonrpc" (server))
(declare-function yas-expand "yasnippet" ())
(declare-function yas-next-field "yasnippet" (&optional arg))
(declare-function riven/xref-find-definitions-or-search "init-helper" ())

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

(defcustom rivenEmacs-eglot-workspace-configuration
  '(:python (:analysis (:typeCheckingMode "basic"))
    :pylsp (:plugins (:jedi_completion (:fuzzy t)))
    :typescript (:preferences (:importModuleSpecifier "relative"))
    :javascript (:preferences (:importModuleSpecifier "relative")))
  "Default LSP workspace settings for RivenEmacs.
This is sent as `eglot-workspace-configuration' to connected servers
unless a project overrides it with directory-local variables.  The
defaults keep Pyright/Pylsp and TypeScript useful without imposing
language-specific project policy:

- `python.analysis.typeCheckingMode': `basic' instead of the server
  default (which may be off), with project pyrightconfig.json still
  taking precedence.
- Pylsp Jedi completion is fuzzy.
- TS/JS prefer relative import paths.

Use `M-x eglot-show-workspace-configuration' to inspect the effective
value, or set this variable to nil to restore Eglot's defaults."
  :type 'sexp
  :group 'rivenEmacs)

(defun riven/eglot-workspace-configuration (&optional _server)
  "Return `rivenEmacs-eglot-workspace-configuration' for Eglot.
The optional SERVER argument is accepted for Eglot's function-form
`eglot-workspace-configuration' contract but ignored."
  rivenEmacs-eglot-workspace-configuration)

(defcustom rivenEmacs-eglot-install-timeout 300
  "Maximum seconds an on-demand language-server install may run."
  :type 'natnum
  :group 'rivenEmacs)

(defcustom rivenEmacs-eglot-disable-file-watch t
  "Whether to disable dynamic LSP file-watching globally.
Some servers behave poorly when Eglot registers
`workspace/didChangeWatchedFiles'; set this to nil to keep the
standard registration for all servers."
  :type 'boolean
  :group 'rivenEmacs)

(defcustom rivenEmacs-eglot-inlay-hint-modes nil
  "Major modes where Eglot inlay hints are enabled automatically.
Inlay hints add server-provided type and parameter annotations.  This
is opt-in because it can be visually noisy and has a per-edit cost.
Set it to a list such as `(rust-ts-mode typescript-ts-mode)' and
reconnect Eglot to apply it to existing buffers."
  :type '(repeat symbol)
  :group 'rivenEmacs)

(defvar riven/eglot--declined-installs (make-hash-table :test #'equal)
  "Language servers the user declined to install in this session.")

(defun riven/eglot-reset-install-prompts ()
  "Forget previously declined language-server installs.
After this command, opening a file with a missing server can prompt
again."
  (interactive)
  (clrhash riven/eglot--declined-installs)
  (message "Eglot install prompt decisions reset."))

(defun riven/eglot--ensure-server (binary install-cmd)
  "Ensure BINARY is on PATH, optionally installing via INSTALL-CMD.
INSTALL-CMD is a list like (\"npm\" \"i\" \"-g\" \"foo\") or
(\"pip\" \"install\" \"foo\"); any installer works since it is run as a
subprocess (no shell).  If BINARY is already on PATH, return t
immediately.  Otherwise, when `rivenEmacs-eglot-auto-install' is
non-nil, prompt in the minibuffer; on `y' run INSTALL-CMD in a
subprocess, keep redisplay alive while it runs, refresh `exec-path'
afterwards, and surface its output.  Declining is remembered for the
session; use `riven/eglot-reset-install-prompts' to ask again.
Returns non-nil if BINARY is available afterwards, nil if the user
declined, install failed, or auto-install is disabled.  If INSTALL-CMD
is nil, only detect."
  (cond
   ((executable-find binary) t)
   ((or (null install-cmd) (not rivenEmacs-eglot-auto-install)) nil)
   ((gethash binary riven/eglot--declined-installs) nil)
   ((not (y-or-n-p (format "Eglot: `%s' not found. Install (%s)? "
                           binary (mapconcat #'identity install-cmd " "))))
    (puthash binary t riven/eglot--declined-installs)
    (message "Install skipped; run M-x riven/eglot-reset-install-prompts to ask again.")
    nil)
   (t
    (message "Running `%s'..." (mapconcat #'identity install-cmd " "))
    (let* ((program (executable-find (car install-cmd)))
           (buf (get-buffer-create "*eglot server install*")))
      (if (not program)
          (progn
            (message "Install FAILED: `%s' is not on PATH; install it manually."
                     (car install-cmd))
            nil)
        (with-current-buffer buf
          (erase-buffer))
        (let ((status (condition-case err
                          (riven/eglot--run-install program (cdr install-cmd) buf)
                        (quit
                         (message "Install cancelled.")
                         nil)
                        (error
                         (message "Install FAILED: %s"
                                  (error-message-string err))
                         nil))))
          (cond
           ((eq status 0)
            (riven/eglot--refresh-exec-path)
            (if (executable-find binary)
                (progn
                  (message "Installed %s." binary)
                  t)
              (display-buffer buf)
              (message "Install finished but `%s' is still not on PATH; check %s."
                       binary (buffer-name buf))
              nil))
           ((null status) nil)
           (t
            (display-buffer buf)
            (message "Install FAILED (exit %s) — see %s."
                     status (buffer-name buf))
            nil))))))))

(defun riven/eglot--run-install (program args buffer)
  "Run PROGRAM with ARGS, writing output to BUFFER; return exit status.
The process is asynchronous internally and `accept-process-output'
keeps redisplay and timers alive while waiting, so an install does not
freeze the whole UI.  Aborts after `rivenEmacs-eglot-install-timeout'
seconds."
  (let* ((proc (make-process :name "eglot-install"
                             :command (cons program args)
                             :buffer buffer
                             :stderr buffer
                             :noquery t))
         (start (float-time))
         (deadline (+ start rivenEmacs-eglot-install-timeout))
         status)
    (unwind-protect
        (progn
          (while (and (process-live-p proc) (< (float-time) deadline))
            (accept-process-output proc 0.2)
            (message "Installing language server... (%.0fs)"
                     (- (float-time) start)))
          (if (process-live-p proc)
              (progn
                (delete-process proc)
                (error "Install timed out after %ss"
                       rivenEmacs-eglot-install-timeout))
            (setq status (process-exit-status proc))))
      (when (process-live-p proc)
        (delete-process proc)))
    status))

(defun riven/eglot--refresh-exec-path ()
  "Add common user and package-manager bin directories to `exec-path'.
This makes a freshly installed npm/pip/cargo server discoverable even
when the GUI Emacs process started with a shorter PATH."
  (let ((dirs (append
               (list (expand-file-name "~/.npm-global/bin")
                     (expand-file-name "~/.local/bin")
                     (expand-file-name "~/bin")
                     (expand-file-name "~/.cargo/bin")
                     (expand-file-name "~/go/bin")
                     "/opt/homebrew/bin"
                     "/usr/local/bin")
               (file-expand-wildcards "~/Library/Python/*/bin"))))
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (add-to-list 'exec-path dir)))
    (setenv "PATH"
            (mapconcat #'identity
                       (delq nil (mapcar (lambda (entry)
                                           (and (stringp entry) entry))
                                         exec-path))
                       path-separator))))

(defun riven/eglot--maybe-enable-inlay-hints ()
  "Enable Eglot inlay hints when the current mode is configured for them."
  (when (and rivenEmacs-eglot-inlay-hint-modes
             (memq major-mode rivenEmacs-eglot-inlay-hint-modes)
             (bound-and-true-p eglot--managed-mode)
             (fboundp 'eglot-inlay-hints-mode)
             (eglot-server-capable :inlayHintProvider))
    (eglot-inlay-hints-mode 1)))

(defun riven/eglot--installed-server (candidates)
  "Return the CONTACT of the first already-installed server in CANDIDATES.
CANDIDATES is a list of (BINARY CONTACT INSTALL-CMD) triples in
preference order; BINARY must be an executable name string (not a
symbol), as required by `executable-find'.  This check never prompts
and never installs: it lets Eglot use a working server before the
user is asked to install a more preferred, but absent, one.  Returns
nil when none is installed."
  (cl-loop for (binary contact _install) in candidates
           when (executable-find binary)
           return contact))

(defun riven/eglot--pick-server (candidates)
  "Return CONTACT for the best available server among CANDIDATES.
CANDIDATES is a list of (BINARY CONTACT INSTALL-CMD) triples, tried
in order; BINARY must be an executable name string (not a symbol), as
required by `executable-find'.  A server that is already installed
wins immediately, so no install prompt appears when e.g. Pyright
exists but BasedPyright does not.  Only when none is installed are
the missing servers offered for installation in preference order, via
`riven/eglot--ensure-server'.  Quitting an install prompt counts as
declining that server and moves on to the next candidate, so a stray
\\[keyboard-quit] cannot leave a buffer without any connection attempt.
Returns the chosen CONTACT, or nil when every candidate is missing and
declined."
  (or (riven/eglot--installed-server candidates)
      (cl-block nil
        (cl-loop for (bin contact install) in candidates
                 when (condition-case nil
                          (riven/eglot--ensure-server bin install)
                        (quit
                         (puthash bin t riven/eglot--declined-installs)
                         (message "Install of `%s' cancelled; trying next server."
                                  bin)
                         nil))
                 do (cl-return contact)))))

(defun riven/eglot-python-contact (&optional _interactive _project)
  "Return preferred Eglot server contact for Python, or nil.
Already-installed servers win in the order basedpyright, pyright,
pylsp, ruff (ruff's LSP is primarily a linter/formatter, so it is the
last resort).  Only when none of them is installed does it offer to
install the missing ones in that preference order.  Returns nil when
no server is installed and the user declines installation; Eglot then
reports that it cannot guess a server instead of silently trying a
missing binary.  Optional arguments are accepted (and ignored) to
satisfy eglot's function-form contact contract (see
`eglot-server-programs')."
  (riven/eglot--pick-server
   '(("basedpyright-langserver" ("basedpyright-langserver" "--stdio")
                                ("pip" "install" "basedpyright"))
     ("pyright-langserver" ("pyright-langserver" "--stdio")
                           ("npm" "i" "-g" "pyright"))
     ("pylsp" ("pylsp") ("pip" "install" "python-lsp-server"))
     ("ruff" ("ruff" "server") ("pip" "install" "ruff")))))

(defun riven/eglot-configure-python-server ()
  "Register the Python LSP for `python-mode' and `python-ts-mode'.
The contact is a function so servers are detected/offered for
installation when a Python buffer first connects."
  (when (boundp 'eglot-server-programs)
    (setf (alist-get '(python-mode :language-id "python")
                     eglot-server-programs nil nil #'equal)
          #'riven/eglot-python-contact)
    (setf (alist-get '(python-ts-mode :language-id "python")
                     eglot-server-programs nil nil #'equal)
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
    (setf (alist-get '(vue-ts-mode :language-id "vue")
                     eglot-server-programs nil nil #'equal)
          #'riven/eglot--vue-contact)))

(defun riven/eglot-kotlin-contact (&optional _interactive _project)
  "Return Eglot server contact for Kotlin, or nil if none can be set up.
Uses `kotlin-lsp' when already installed; otherwise offers to install
`kotlin-language-server' via npm.  Returns the contact of the first
available server, or nil if the user declines.  Optional arguments are
accepted (and ignored) to satisfy eglot's function-form contact
contract."
  (or (riven/eglot--pick-server
       '(("kotlin-lsp" ("kotlin-lsp" "--stdio") nil)
         ("kotlin-language-server" ("kotlin-language-server")
                                   ("npm" "i" "-g" "kotlin-language-server"))))
      (error "No Kotlin language server available")))

(defun riven/eglot-configure-kotlin-server ()
  "Register the Kotlin LSP for `kotlin-mode' and `kotlin-ts-mode'.
The contact is a function so servers are detected/offered for
installation when a Kotlin buffer first connects."
  (when (boundp 'eglot-server-programs)
    (setf (alist-get '(kotlin-mode :language-id "kotlin")
                     eglot-server-programs nil nil #'equal)
          #'riven/eglot-kotlin-contact)
    (setf (alist-get '(kotlin-ts-mode :language-id "kotlin")
                     eglot-server-programs nil nil #'equal)
          #'riven/eglot-kotlin-contact)))

(defun riven/eglot-register-simple-server (modes binary contact install-cmd &optional language-id)
  "Register a function-form CONTACT for MODES using BINARY + INSTALL-CMD.
MODES is a list of major-mode symbols.  BINARY is the executable to
detect; CONTACT is the eglot contact list (e.g. (\"clangd\")); INSTALL-CMD
is a list run on user consent to install BINARY (e.g.
(\"brew\" \"install\" \"llvm\")).  If BINARY is missing at connect time
the user is offered INSTALL-CMD.  If INSTALL-CMD is nil the server is
only detected, not installed.  LANGUAGE-ID, when non-nil, is sent as
the LSP language id (useful for e.g. `web-mode' -> \"html\")."
  (when (boundp 'eglot-server-programs)
    (let ((fn (lambda (&optional _interactive _project)
                (if (riven/eglot--ensure-server binary install-cmd)
                    contact
                  (error "`%s' not installed" binary)))))
      (dolist (mode modes)
        (let ((key (if language-id
                       (list mode :language-id language-id)
                     mode)))
          (setf (alist-get key eglot-server-programs nil nil #'equal) fn))))))

(defun riven/eglot-configure-default-servers ()
  "Register common LSPs with on-demand install for built-in modes.
Each entry below overrides eglot's built-in contact for that mode
with a function-form one that prompts to install the server if it
is missing.  Modes not listed here keep eglot's defaults (no
auto-install)."
  (riven/eglot-register-simple-server
   '(rust-mode rust-ts-mode) "rust-analyzer" '("rust-analyzer")
   '("rustup" "component" "add" "rust-analyzer")
   "rust")
  (riven/eglot-register-simple-server
   '(c-mode c-ts-mode) "clangd" '("clangd")
   '("brew" "install" "llvm") "c")
  (riven/eglot-register-simple-server
   '(c++-mode c++-ts-mode) "clangd" '("clangd")
   '("brew" "install" "llvm") "c++")
  (riven/eglot-register-simple-server
   '(objc-mode) "clangd" '("clangd")
   '("brew" "install" "llvm") "objective-c")
  (riven/eglot-register-simple-server
   '(java-mode java-ts-mode) "jdtls" '("jdtls")
   '("brew" "install" "jdtls") "java")
  (riven/eglot-register-simple-server
   '(php-mode php-ts-mode) "phpactor" '("phpactor" "language-server")
   '("composer" "global" "require" "phpactor/phpactor") "php")
  ;; JS/TS via typescript-language-server.  Preserve the language ids Eglot's
  ;; built-in contacts use; otherwise -ts modes would send \"js-ts\" etc.
  (riven/eglot-register-simple-server
   '(js-ts-mode) "typescript-language-server"
   '("typescript-language-server" "--stdio")
   '("npm" "i" "-g" "typescript-language-server" "typescript")
   "javascript")
  (riven/eglot-register-simple-server
   '(typescript-ts-mode) "typescript-language-server"
   '("typescript-language-server" "--stdio")
   '("npm" "i" "-g" "typescript-language-server" "typescript")
   "typescript")
  (riven/eglot-register-simple-server
   '(tsx-ts-mode) "typescript-language-server"
   '("typescript-language-server" "--stdio")
   '("npm" "i" "-g" "typescript-language-server" "typescript")
   "typescriptreact")
  ;; HTML templates edited with `web-mode' get the HTML language server.
  ;; The server is installed together with the CSS/JSON servers by
  ;; `vscode-langservers-extracted'.
  (riven/eglot-register-simple-server
   '(web-mode) "vscode-html-language-server"
   '("vscode-html-language-server" "--stdio")
   '("npm" "i" "-g" "vscode-langservers-extracted")
   "html")
  (riven/eglot-register-simple-server
   '(html-mode html-ts-mode) "vscode-html-language-server"
   '("vscode-html-language-server" "--stdio")
   '("npm" "i" "-g" "vscode-langservers-extracted")
   "html")
  (riven/eglot-register-simple-server
   '(css-mode css-ts-mode) "vscode-css-language-server"
   '("vscode-css-language-server" "--stdio")
   '("npm" "i" "-g" "vscode-langservers-extracted")
   "css")
  ;; JSON files are edited with `js-json-mode' in Emacs 31; JSONC is used by
  ;; VS Code-style config files.
  (riven/eglot-register-simple-server
   '(js-json-mode json-mode json-ts-mode)
   "vscode-json-language-server"
   '("vscode-json-language-server" "--stdio")
   '("npm" "i" "-g" "vscode-langservers-extracted")
   "json")
  (riven/eglot-register-simple-server
   '(jsonc-mode) "vscode-json-language-server"
   '("vscode-json-language-server" "--stdio")
   '("npm" "i" "-g" "vscode-langservers-extracted")
   "jsonc")
  ;; YAML language server is smaller and installed on demand.
  (riven/eglot-register-simple-server
   '(yaml-mode yaml-ts-mode yaml-ts-mode-maybe)
   "yaml-language-server" '("yaml-language-server" "--stdio")
   '("npm" "i" "-g" "yaml-language-server")
   "yaml")
  ;; Go, Lua and Bash get the same on-demand treatment.
  (riven/eglot-register-simple-server
   '(go-mode go-ts-mode) "gopls" '("gopls")
   '("go" "install" "golang.org/x/tools/gopls@latest")
   "go")
  (riven/eglot-register-simple-server
   '(lua-mode lua-ts-mode) "lua-language-server" '("lua-language-server")
   '("brew" "install" "lua-language-server")
   "lua")
  (riven/eglot-register-simple-server
   '(bash-ts-mode sh-mode) "bash-language-server"
   '("bash-language-server" "start")
   '("npm" "i" "-g" "bash-language-server")
   "shellscript"))

(defun riven/eglot--process-command-string (server)
  "Return SERVER's process command line and status, or nil.
SERVER may be nil, in which case nil is returned."
  (when-let* ((process (ignore-errors (jsonrpc--process server))))
    (format "%s [%s]"
            (ignore-errors
              (mapconcat #'identity (process-command process) " "))
            (ignore-errors (process-status process)))))

(defun riven/eglot-diagnose ()
  "Report Eglot and in-buffer completion state for the current buffer.
Run this when completion does not appear.  It distinguishes the cases
that look alike in the mode line: Eglot is not managing the buffer, it
claims to manage it but has no live server, the server is alive but
`eglot-completion-at-point' is missing from
`completion-at-point-functions', or the server is alive but the
completion backend returns nothing (for Rust this usually means there
is no Cargo project).  The report is shown in *Riven LSP Diagnostics*."
  (interactive)
  (let* ((server (ignore-errors (eglot-current-server)))
         (managed (and (bound-and-true-p eglot--managed-mode) t))
         (capf (and (boundp 'completion-at-point-functions)
                    completion-at-point-functions))
         (corfu-on (and (boundp 'corfu-mode)
                        (bound-and-true-p corfu-mode)))
         (corfu-auto-on (and (boundp 'corfu-auto) corfu-auto))
         (auto-hook (and (boundp 'post-command-hook)
                         (memq 'corfu-auto--post-command post-command-hook)))
         (rust-standalone
          (and (derived-mode-p 'rust-ts-mode 'rust-mode)
               (not (locate-dominating-file default-directory "Cargo.toml"))
               (not (locate-dominating-file default-directory "rust-project.json"))))
         (report
          (list
           (format "buffer: %s (%s)" (buffer-name) major-mode)
           (format "file: %s" (or buffer-file-name "(none)"))
           (format "project: %s"
                   (or (ignore-errors
                         (project-root (project-current)))
                       "(none)"))
           (format "Eglot managed: %s" (if managed "yes" "no"))
           (format "Eglot server: %s" (if server "yes" "no"))
           (format "server process: %s"
                   (or (and server (riven/eglot--process-command-string server))
                       "(none)"))
           (format "capf: %S" capf)
           (format "corfu-mode: %s  corfu-auto: %s  auto hook: %s"
                   (if corfu-on "on" "off")
                   (if corfu-auto-on "on" "off")
                   (if auto-hook "present" "missing"))
           (format "mouse hover: %s"
                   (if (bound-and-true-p riven/eglot-mouse-hover-mode)
                       "on" "off"))
           (format "rust cargo project: %s" (if rust-standalone "no" "yes")))))
    (with-current-buffer (get-buffer-create "*Riven LSP Diagnostics*")
      (erase-buffer)
      (insert "RivenEmacs LSP diagnostics\n"
              (make-string 32 ?-) "\n")
      (dolist (line report)
        (insert line "\n"))
      (insert "\nNotes:\n")
      (unless managed
        (insert "  - Eglot does not manage this buffer, so LSP completion is\n"
                "    unavailable.  Check the *EGLOT (...) events* buffer for\n"
                "    connection/install errors, then retry `M-x eglot-ensure'.\n"))
      (when (and managed (null server))
        (insert "  - Eglot reports a managed buffer but has no live server.\n"
                "    Run `M-x eglot-reconnect'.\n"))
      (when (and server capf (not (memq #'eglot-completion-at-point capf)))
        (insert "  - The server is alive but its completion back-end is not in\n"
                "    `completion-at-point-functions'.  Run `M-x eglot-reconnect'.\n"))
      (when (and server (not corfu-on))
        (insert "  - Corfu is disabled in this buffer; `M-x corfu-mode' enables it.\n"))
      (when (and corfu-auto-on (not auto-hook))
        (insert "  - Corfu auto-popup hook is missing; toggle `M-x corfu-mode'\n"
                "    twice (or restart Emacs) to rebuild it.\n"))
      (when (and server rust-standalone)
        (insert "  - rust-analyzer without Cargo.toml/rust-project.json runs in\n"
                "    detached-file mode and returns no completions.  Start the\n"
                "    problem again with `C-c C-c' in its statement buffer, or\n"
                "    add a Cargo.toml whose [[bin]] points at this file.\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer)))
    (message "LSP diagnostics written to *Riven LSP Diagnostics*")))

;;; Mouse-hover documentation
;;
;; Eglot exposes hover support through ElDoc, which is bound to the text at
;; point.  These members add the mouse equivalent: resting the pointer on a
;; symbol in a managed buffer asks the server for hover documentation after a
;; short idle delay and shows it in the echo area.

(defcustom rivenEmacs-eglot-mouse-hover t
  "Whether resting the mouse on a symbol shows its hover documentation.
When non-nil, `riven/eglot-mouse-hover-mode' is enabled after Eglot
loads: a `mouse-movement' binding asks the language server for hover
information and shows it in the echo area after
`rivenEmacs-eglot-mouse-hover-delay' seconds."
  :type 'boolean
  :group 'rivenEmacs)

(defcustom rivenEmacs-eglot-mouse-hover-delay 0.25
  "Idle seconds before mouse-hover documentation is requested."
  :type 'number
  :group 'rivenEmacs)

(defcustom rivenEmacs-eglot-mouse-hover-max-lines 6
  "Maximum number of hover documentation lines shown in the echo area.
Longer documentation is truncated; use `M-x eldoc-doc-buffer' for the
full text of the symbol at point."
  :type 'natnum
  :group 'rivenEmacs)

(defvar riven/eglot-mouse-hover--timer nil
  "Pending idle timer for mouse-hover documentation.")

(defvar riven/eglot-mouse-hover--token 0
  "Counter used to discard out-of-order hover responses.")

(defvar riven/eglot-mouse-hover--last nil
  "Identifier of the last hovered symbol, or nil.")

(defvar riven/eglot-mouse-hover--shown nil
  "Non-nil when the echo area currently shows hover documentation.")

(defun riven/eglot-mouse-hover--cancel ()
  "Cancel a pending hover request and invalidate in-flight responses."
  (when (timerp riven/eglot-mouse-hover--timer)
    (cancel-timer riven/eglot-mouse-hover--timer))
  (setq riven/eglot-mouse-hover--timer nil)
  (setq riven/eglot-mouse-hover--token (1+ riven/eglot-mouse-hover--token)))

(defun riven/eglot-mouse-hover--symbol-at (posn)
  "Return (BUFFER POSITION SYMBOL) for the symbol at mouse position POSN.
Return nil when POSN is not over a symbol in an Eglot-managed buffer."
  (when-let* ((window (posn-window posn))
              ((window-live-p window))
              (position (posn-point posn))
              ((integer-or-marker-p position))
              (buffer (window-buffer window)))
    (with-current-buffer buffer
      (when (and (bound-and-true-p eglot--managed-mode)
                 (eglot-current-server))
        (save-excursion
          (goto-char position)
          (when-let* ((bounds (bounds-of-thing-at-point 'symbol)))
            (list buffer (car bounds)
                  (buffer-substring-no-properties (car bounds) (cdr bounds)))))))))

(defun riven/eglot-mouse-hover--code-fence-p (line)
  "Return non-nil when LINE is a Markdown code fence."
  (string-match-p "\\`[ \t]*```[ \t]*[[:alnum:]_+.-]*[ \t]*\\'" line))

(defun riven/eglot-mouse-hover--format (text)
  "Return TEXT shortened and cleaned up for the echo area."
  (let* ((lines (split-string (string-trim text "[ \t\n\r]+" "[ \t\n\r]+") "\n"))
         (cleaned (seq-remove #'riven/eglot-mouse-hover--code-fence-p lines))
         (kept (seq-take cleaned rivenEmacs-eglot-mouse-hover-max-lines))
         (omitted (- (length cleaned) (length kept))))
    (concat (mapconcat #'string-trim-right kept "\n")
            (when (> omitted 0)
              (format "\n... %d more line(s)" omitted)))))

(defun riven/eglot-mouse-hover--hide ()
  "Clear hover documentation from the echo area when it is shown there."
  (when riven/eglot-mouse-hover--shown
    (message nil)
    (setq riven/eglot-mouse-hover--shown nil)))

(defun riven/eglot-mouse-hover--fetch (info)
  "Request and display hover documentation described by INFO.
INFO is the list returned by `riven/eglot-mouse-hover--symbol-at'."
  (setq riven/eglot-mouse-hover--timer nil)
  (let ((buffer (nth 0 info))
        (position (nth 1 info))
        (token riven/eglot-mouse-hover--token))
    (when (and (buffer-live-p buffer)
               (not (equal info riven/eglot-mouse-hover--last)))
      (setq riven/eglot-mouse-hover--last info)
      (with-current-buffer buffer
        (save-excursion
          (goto-char position)
          (when (and (bound-and-true-p eglot--managed-mode)
                     (eglot-current-server))
            (eglot-hover-eldoc-function
             (lambda (text &rest _plist)
               (when (= token riven/eglot-mouse-hover--token)
                 (if (and (stringp text)
                          (not (string-empty-p (string-trim text))))
                     (progn
                       (message "%s" (riven/eglot-mouse-hover--format text))
                       (setq riven/eglot-mouse-hover--shown t))
                   (riven/eglot-mouse-hover--hide)))))))))))

(defun riven/eglot-mouse-hover (event)
  "Show Eglot documentation for the symbol under the mouse.
This is the `mouse-movement' handler installed by
`riven/eglot-mouse-hover-mode'; documentation appears in the echo area
after `rivenEmacs-eglot-mouse-hover-delay' idle seconds."
  (interactive "e")
  (riven/eglot-mouse-hover--cancel)
  (if-let* ((info (riven/eglot-mouse-hover--symbol-at (event-start event))))
      (setq riven/eglot-mouse-hover--timer
            (run-with-idle-timer rivenEmacs-eglot-mouse-hover-delay nil
                                 #'riven/eglot-mouse-hover--fetch info))
    (setq riven/eglot-mouse-hover--last nil)
    (riven/eglot-mouse-hover--hide)))

(define-minor-mode riven/eglot-mouse-hover-mode
  "Show Eglot hover documentation when the mouse rests on a symbol.
The documentation is displayed in the echo area after
`rivenEmacs-eglot-mouse-hover-delay' seconds.  Enabled at startup when
`rivenEmacs-eglot-mouse-hover' is non-nil; toggle with
\\[riven/eglot-mouse-hover-mode]."
  :global t
  :group 'rivenEmacs
  (cond
   ((not (boundp 'eglot-mode-map))
    nil)
   (riven/eglot-mouse-hover-mode
    (keymap-set eglot-mode-map "<mouse-movement>" #'riven/eglot-mouse-hover))
   ((eq (lookup-key eglot-mode-map [mouse-movement])
        #'riven/eglot-mouse-hover)
    (keymap-unset eglot-mode-map "<mouse-movement>")
    (riven/eglot-mouse-hover--cancel)
    (riven/eglot-mouse-hover--hide))))

;;; Signature-help overload cycling
;;
;; Eglot's ElDoc signature help echoes only the server's active signature (the
;; `:echo' hint).  These commands cycle through the other overloads of the
;; call at point and echo the selected one; the full list stays available in
;; the ElDoc buffer (`C-c c h').

(defvar riven/eglot-signature--state nil
  "Cycle state for signature overloads.
A list (BUFFER POINT TICK INDEX TOTAL) describing the last echoed
overload, or nil.")

(defun riven/eglot-signature--cycle (delta)
  "Cycle DELTA positions through the overloads at point and echo the result."
  (let ((server (eglot-current-server)))
    (unless (and server (bound-and-true-p eglot--managed-mode))
      (user-error "No Eglot server in this buffer"))
    (unless (eglot-server-capable :signatureHelpProvider)
      (user-error "The language server provides no signature help"))
    (let* ((response
            (condition-case err
                (eglot--request
                 server :textDocument/signatureHelp
                 (list :textDocument (eglot--TextDocumentIdentifier)
                       :position (eglot--pos-to-lsp-position))
                 :timeout 3
                 :cancel-on-input t)
              (error
               (user-error "Signature help failed: %s"
                           (error-message-string err)))))
           (signatures (plist-get response :signatures))
           (active (or (plist-get response :activeSignature) 0))
           (active-parameter (or (plist-get response :activeParameter) 0)))
      (if (or (null signatures) (zerop (length signatures)))
          (message "No signature help at point")
        (let* ((total (length signatures))
               (key (list (current-buffer) (point)
                          (buffer-chars-modified-tick) total))
               (previous (when (equal (seq-take riven/eglot-signature--state 4)
                                      key)
                           (nth 4 riven/eglot-signature--state)))
               (index (mod (if previous
                               (+ previous delta)
                             (+ active delta))
                           total)))
          (setq riven/eglot-signature--state (append key (list index)))
          (message "%s"
                   (concat (format "[%d/%d] " (1+ index) total)
                           (eglot--sig-info (aref signatures index)
                                            active-parameter t))))))))

(defun riven/eglot-signature-next (&optional n)
  "Echo the next N signature overload of the call at point.
The selected overload is shown in the echo area as \"[i/n] SIGNATURE\"
and the active parameter is highlighted.  Press repeatedly to cycle.
The full list with parameter documentation is available in the ElDoc
buffer (`\\[eldoc-doc-buffer]', also on `C-c c h')."
  (interactive "p")
  (riven/eglot-signature--cycle (or n 1)))

(defun riven/eglot-signature-previous (&optional n)
  "Echo the previous N signature overload of the call at point.
The counterpart of `riven/eglot-signature-next'."
  (interactive "p")
  (riven/eglot-signature--cycle (- (or n 1))))

(defun riven/eglot-client-capabilities-no-file-watch (orig-fn server)
  "Call ORIG-FN for SERVER, disabling dynamic file watch when configured.
The default is to disable the registration; set
`rivenEmacs-eglot-disable-file-watch' to nil to keep Eglot's normal
`workspace/didChangeWatchedFiles' behavior."
  (let ((caps (funcall orig-fn server)))
    (if (not rivenEmacs-eglot-disable-file-watch)
        caps
      (let* ((workspace (plist-get caps :workspace))
             (watch (and (listp workspace)
                         (plist-get workspace :didChangeWatchedFiles))))
        (when (listp watch)
          (setq watch (plist-put watch :dynamicRegistration :json-false))
          (setq workspace (plist-put workspace :didChangeWatchedFiles watch))
          (setq caps (plist-put caps :workspace workspace)))
        caps))))

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
              ("M-?" . xref-find-references)
              ;; Signature-help overload cycling.  The minor-mode map takes
              ;; precedence over major-mode bindings (e.g. cc-mode's
              ;; `C-c C-n'/`C-c C-p' conditional navigation and python-mode's
              ;; `C-c C-p' run-python), so these only exist in LSP buffers.
              ("C-c C-n" . riven/eglot-signature-next)
              ("C-c C-p" . riven/eglot-signature-previous))
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
  :config
  ;; on-type formatting makes a synchronous blocking request to the server,
  ;; causing Emacs to freeze on characters like `;' and RET.  Keep it off;
  ;; format-on-save is handled by Apheleia/LSP formatting commands instead.
  (setq eglot-ignored-server-capabilities
        '(:documentOnTypeFormattingProvider))
  (setq-default eglot-workspace-configuration
                #'riven/eglot-workspace-configuration)
  (add-hook 'eglot-managed-mode-hook #'riven/eglot--maybe-enable-inlay-hints)
  (riven/eglot-configure-python-server)
  (riven/eglot-configure-vue-server)
  (riven/eglot-configure-kotlin-server)
  (riven/eglot-configure-default-servers)
  (setq-default jsonrpc-default-request-timeout 30)
  ;; Mouse-hover documentation; see `riven/eglot-mouse-hover-mode'.
  (when rivenEmacs-eglot-mouse-hover
    (riven/eglot-mouse-hover-mode 1))
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
