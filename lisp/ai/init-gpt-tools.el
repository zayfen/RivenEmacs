;;; init-gpt-tools.el --- Core gptel tools and Work preset -*- lexical-binding: t; -*-

;;; Commentary:

;; ChatGPT Work is a ChatGPT product mode with no public API.  This module
;; gives gptel a local equivalent: a small, confined set of filesystem,
;; grep, shell, and web tools plus a `work' preset that instructs the model
;; to plan, inspect, edit, and verify changes like an agent.
;;
;; Tools are registered in gptel's global tool registry (M-x gptel-tools)
;; and are only activated by the `work' preset (C-c a W or "@work" in a
;; prompt).  Destructive tools (`write_file', `run_shell_command') always
;; ask for confirmation, and every path must stay below an allowed root.
;;
;; This file is loaded from init-gpt.el after gptel is available.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'shr)
(require 'subr-x)
(require 'url)
(require 'gptel)

(declare-function project-root "project" (project))

(defcustom rivenEmacs-gptel-tool-roots nil
  "Directories that gptel core tools may access.
When nil, use the current project root, then
`rivenEmacs-default-workspace', then `default-directory'."
  :type '(repeat directory)
  :group 'rivenEmacs)

(defcustom rivenEmacs-gptel-tool-max-file-bytes 200000
  "Maximum file size `read_file' will return."
  :type 'integer
  :group 'rivenEmacs)

(defcustom rivenEmacs-gptel-tool-max-output-chars 20000
  "Maximum number of characters returned by a core tool."
  :type 'integer
  :group 'rivenEmacs)

(defun riven/gptel-tool--root ()
  "Return the directory that confines the gptel core tools."
  (expand-file-name
   (or (car-safe rivenEmacs-gptel-tool-roots)
       (when-let* ((project (ignore-errors
                              (project-current nil default-directory))))
         (project-root project))
       (and (boundp 'rivenEmacs-default-workspace)
            rivenEmacs-default-workspace)
       default-directory)))

(defun riven/gptel-tool--path (path)
  "Resolve PATH below the allowed root, rejecting paths that escape it."
  (let* ((root (file-name-as-directory
                (file-truename (riven/gptel-tool--root))))
         (expanded (expand-file-name (or path ".") root))
         (true (file-truename expanded)))
    (unless (string-prefix-p root true)
      (error "Path %S is outside the allowed root %S" path root))
    true))

(defun riven/gptel-tool--truncate (text)
  "Truncate TEXT to `rivenEmacs-gptel-tool-max-output-chars'."
  (if (> (length text) rivenEmacs-gptel-tool-max-output-chars)
      (concat (substring text 0 rivenEmacs-gptel-tool-max-output-chars)
              "\n...[truncated]")
    text))

(defun riven/gptel-tool--read-file (path)
  "Return the contents of PATH."
  (let* ((file (riven/gptel-tool--path path))
         (attrs (file-attributes file)))
    (unless (file-regular-p file)
      (error "Not a regular file: %s" path))
    (when (> (file-attribute-size attrs)
             rivenEmacs-gptel-tool-max-file-bytes)
      (error "File %s exceeds the %d-byte limit"
             path rivenEmacs-gptel-tool-max-file-bytes))
    (with-temp-buffer
      (insert-file-contents file)
      (riven/gptel-tool--truncate (buffer-string)))))

(defun riven/gptel-tool--list-directory (path)
  "Return a newline-separated listing of PATH."
  (let ((dir (riven/gptel-tool--path path)))
    (unless (file-directory-p dir)
      (error "Not a directory: %s" path))
    (let ((entries (sort (seq-difference (directory-files dir)
                                         '("." ".."))
                         #'string-lessp)))
      (format "%s\n%s"
              dir
              (mapconcat
               (lambda (name)
                 (if (file-directory-p (expand-file-name name dir))
                     (concat name "/")
                   name))
               entries "\n")))))

(defun riven/gptel-tool--grep (pattern path)
  "Search for PATTERN under PATH using ripgrep."
  (let* ((root (riven/gptel-tool--root))
         (target (if path (riven/gptel-tool--path path) root))
         (rg (executable-find "rg")))
    (unless rg
      (error "ripgrep (rg) is not installed"))
    (with-temp-buffer
      (let ((default-directory (file-name-as-directory root))
            (coding-system-for-read 'utf-8))
        (apply #'call-process rg nil t nil
               "--line-number" "--no-heading" "--color=never"
               "--max-count" "5" "--" pattern target))
      (riven/gptel-tool--truncate
       (replace-regexp-in-string
        (concat "\\`" (regexp-quote (file-name-as-directory root)))
        "" (buffer-string))))))

(defun riven/gptel-tool--write-file (path content)
  "Write CONTENT to PATH, creating parent directories when needed."
  (let ((file (riven/gptel-tool--path path)))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (insert content)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max) file nil 'silent)))
    (format "Wrote %d characters to %s" (length content) file)))

(defun riven/gptel-tool--run-shell (command)
  "Run COMMAND in the allowed root and return its combined output."
  (let ((default-directory (file-name-as-directory (riven/gptel-tool--root))))
    (with-timeout (60 (error "Shell command timed out after 60 seconds"))
      (riven/gptel-tool--truncate
       (shell-command-to-string command)))))

(defun riven/gptel-tool--web-search (query &optional max-results)
  "Search the web for QUERY with Tavily and return MAX-RESULTS summaries."
  (let ((key (or (and (fboundp 'riven/gptel--read-env)
                      (riven/gptel--read-env "TAVILY_API_KEY"))
                 (getenv "TAVILY_API_KEY"))))
    (unless (and key (not (string-empty-p key)))
      (error "TAVILY_API_KEY is not set; cannot use web_search"))
    (let* ((url-request-method "POST")
           (url-request-extra-headers
            '(("Content-Type" . "application/json")))
           (url-request-data
            (json-encode
             `(("api_key" . ,key)
               ("query" . ,query)
               ("max_results" . ,(or max-results 5))
               ("include_answer" . t)))))
      (with-current-buffer
          (url-retrieve-synchronously "https://api.tavily.com/search"
                                      t t 30)
        (goto-char (point-min))
        (unless (re-search-forward "\n\n" nil t)
          (error "Tavily returned an invalid response"))
        (let* ((json-object-type 'alist)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (data (json-read))
               (answer (alist-get 'answer data))
               (results (alist-get 'results data)))
          (riven/gptel-tool--truncate
           (concat
            (when answer (format "Answer: %s\n\n" answer))
            (mapconcat
             (lambda (result)
               (format "- %s\n  %s\n  %s"
                       (alist-get 'title result)
                       (alist-get 'url result)
                       (alist-get 'content result)))
             results "\n"))))))))

(defun riven/gptel-tool--read-url (url)
  "Fetch URL and return readable text."
  (unless (string-match-p "\\`https?://" url)
    (error "Only http(s) URLs are supported: %s" url))
  (let ((buffer (url-retrieve-synchronously url t t 30)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (unless (re-search-forward "\n\n" nil t)
            (error "Could not parse %s" url))
          (if (fboundp 'libxml-parse-html-region)
              (with-temp-buffer
                (shr-insert-document
                 (libxml-parse-html-region (point) (point-max)))
                (riven/gptel-tool--truncate (buffer-string)))
            (riven/gptel-tool--truncate
             (buffer-substring-no-properties (point) (point-max)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun riven/gptel-tool--register-core-tools ()
  "Register the gptel core tools (filesystem, shell, web)."
  (gptel-make-tool
   :name "read_file"
   :function #'riven/gptel-tool--read-file
   :description "Read a UTF-8 text file below the configured workspace root."
   :args '((:name "path" :type string
            :description "File path, absolute or relative to the workspace root"))
   :category "riven-core"
   :include 'call)

  (gptel-make-tool
   :name "list_directory"
   :function #'riven/gptel-tool--list-directory
   :description "List entries in a directory below the workspace root."
   :args '((:name "path" :type string :optional t
            :description "Directory path; defaults to the workspace root"))
   :category "riven-core"
   :include 'call)

  (gptel-make-tool
   :name "grep_files"
   :function #'riven/gptel-tool--grep
   :description "Search for a regex pattern with ripgrep and return matching lines."
   :args '((:name "pattern" :type string
            :description "Ripgrep regular expression")
           (:name "path" :type string :optional t
            :description "File or directory to search; defaults to the workspace root"))
   :category "riven-core"
   :include 'call)

  (gptel-make-tool
   :name "write_file"
   :function #'riven/gptel-tool--write-file
   :description "Create or overwrite a text file below the workspace root. Requires confirmation."
   :args '((:name "path" :type string
            :description "File path, absolute or relative to the workspace root")
           (:name "content" :type string
            :description "Complete new file contents"))
   :category "riven-core"
   :confirm t
   :include 'call)

  (gptel-make-tool
   :name "run_shell_command"
   :function #'riven/gptel-tool--run-shell
   :description "Run a shell command in the workspace root. Requires confirmation."
   :args '((:name "command" :type string
            :description "Shell command to execute"))
   :category "riven-core"
   :confirm t
   :include 'call)

  (gptel-make-tool
   :name "web_search"
   :function #'riven/gptel-tool--web-search
   :description "Search the web with Tavily. Requires TAVILY_API_KEY."
   :args '((:name "query" :type string
            :description "Search query")
           (:name "max_results" :type integer :optional t
            :description "Number of results, default 5"))
   :category "riven-core"
   :include 'call)

  (gptel-make-tool
   :name "read_url"
   :function #'riven/gptel-tool--read-url
   :description "Fetch a URL and return readable page text."
   :args '((:name "url" :type string
            :description "HTTP(S) URL to read"))
   :category "riven-core"
   :include 'call))

(defconst riven/gptel-work-system-prompt
  "You are working in ChatGPT Work style: act like a careful local agent.

Workflow:
1. Inspect before editing: read relevant files and search the workspace.
2. Make a short plan, then execute it with the provided tools.
3. Prefer small, reviewable changes. Use write_file for complete file contents.
4. Use run_shell_command only when necessary; destructive commands are
   confirmed by the user.
5. Verify your work (grep, read back files, run tests when available) and
   summarize what changed, which files were touched, and any remaining risk.

Use web_search/read_url for up-to-date external information and cite URLs.
Never claim a tool ran successfully unless its result says so."
  "System prompt for the gptel `work' preset.")

(defun riven/gptel-tools-register ()
  "Register the gptel core tools and the `work' preset."
  (riven/gptel-tool--register-core-tools)
  (gptel-make-preset
   'work
   :description "ChatGPT Work-style local agent (files, shell, grep, web)"
   :system riven/gptel-work-system-prompt
   :use-tools t
   :tools '("read_file" "list_directory" "grep_files" "write_file"
            "run_shell_command" "web_search" "read_url")
   :confirm-tool-calls 'auto
   :include-tool-results 'call))

(riven/gptel-tools-register)

(provide 'init-gpt-tools)
;;; init-gpt-tools.el ends here
