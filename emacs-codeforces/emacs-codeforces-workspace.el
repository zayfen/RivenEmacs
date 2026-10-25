;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-workspace.el --- Local workspace + templates -*- lexical-binding: t; -*-

;;; Commentary:
;; Per-problem directories under `~/.emacs-codeforces/workspace/' and language
;; template instantiation.  Pure local-filesystem, no network.

;;; Code:

;; Declared as `defcustom' in emacs-codeforces.el; defvar here so this module
;; can be loaded and tested standalone.
(defvar codeforces-home-directory
  (expand-file-name "~/.emacs-codeforces/")
  "Codeforces workspace root.
Also a `defcustom' in `emacs-codeforces.el'; kept here so this module
works when loaded standalone (the `defcustom' will not overwrite an
already-set value).")

(defvar codeforces-templates-directory nil
  "Override directory for language templates.  Set by `emacs-codeforces.el'.")

(defconst +cf--module-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory containing this module's source file (captured at load time).")

(defun +cf--templates-dir ()
  "Return the templates directory (override or bundled)."
  (or codeforces-templates-directory
      (expand-file-name "templates" +cf--module-dir)))

(defun +cf--language-extension (language)
  "Return the file extension for LANGUAGE (\"rust\" -> \"rs\")."
  (pcase language
    ("rust" "rs")
    ("python" "py")
    ("cpp" "cpp")
    ("java" "java")
    (_ (error "Unknown language: %s" language))))

(defun +cf--language-template-file (language)
  "Return the template file path for LANGUAGE."
  (expand-file-name
   (pcase language
     ("rust" "rust.rs")
     ("python" "python.py")
     ("cpp" "cpp.cpp")
     ("java" "java.java")
     (_ (error "Unknown language: %s" language)))
   (+cf--templates-dir)))

(defun +cf--ws-problem-id (problem)
  "Return \"contestId_index\" for PROBLEM (used as workspace dir name)."
  (format "%s_%s" (plist-get problem :contestId) (plist-get problem :index)))

(defun +cf--problem-dir (problem)
  "Return the per-problem workspace directory."
  (expand-file-name (+cf--ws-problem-id problem)
                    (expand-file-name "workspace" codeforces-home-directory)))

(defconst +cf--rust-cargo-template
  "[package]
name = \"%s\"
version = \"0.1.0\"
edition = \"2021\"

[[bin]]
name = \"solution\"
path = \"solution.rs\"
"
  "Cargo manifest written for a Rust workspace.
rust-analyzer only provides type-aware completion and diagnostics
inside a Cargo project; a manifest whose single `[[bin]]' points at
the workspace's `solution.rs' keeps the familiar flat file layout.")

(defun +cf--rust-package-name (problem)
  "Return a valid Cargo package name for PROBLEM."
  (let ((raw (format "cf-%s-%s"
                     (plist-get problem :contestId)
                     (plist-get problem :index))))
    (downcase (replace-regexp-in-string "[^[:alnum:]_-]" "-" raw))))

(defun +cf--ensure-rust-project (problem)
  "Ensure PROBLEM's Rust workspace has a Cargo manifest.
Without a Cargo project rust-analyzer runs in its limited
detached-file mode, so LSP completion silently returns nothing even
though Eglot reports the buffer as managed.  Create `Cargo.toml' when
it is missing or blank (for example a 0-byte placeholder), but never
rewrite an existing non-blank manifest.  Return the manifest path."
  (let* ((dir (+cf--problem-dir problem))
         (manifest (expand-file-name "Cargo.toml" dir))
         (blank (or (not (file-exists-p manifest))
                    (with-temp-buffer
                      (insert-file-contents manifest)
                      (string-blank-p (buffer-string))))))
    (when blank
      (with-temp-file manifest
        (insert (format +cf--rust-cargo-template
                        (+cf--rust-package-name problem)))))
    manifest))

(defun +cf-init-solution (problem language)
  "Create PROBLEM's workspace dir and init solution.<ext> from LANGUAGE template.
Returns the path to the solution file.  Never overwrites an existing solution.
For Rust, also ensure a Cargo manifest exists so rust-analyzer can offer
type-aware completion (see `+cf--ensure-rust-project')."
  (let* ((dir (+cf--problem-dir problem))
         (ext (+cf--language-extension language))
         (sol (expand-file-name (concat "solution." ext) dir)))
    (make-directory dir t)
    (unless (file-exists-p sol)
      (let ((tpl (+cf--language-template-file language)))
        (with-temp-file sol
          (insert-file-contents tpl)
          (goto-char (point-min))
          (while (search-forward "{problem_id}" nil t)
            (replace-match (+cf--ws-problem-id problem) t t)))))
    (when (equal language "rust")
      (+cf--ensure-rust-project problem))
    sol))

(provide 'emacs-codeforces-workspace)

;;; emacs-codeforces-workspace.el ends here
