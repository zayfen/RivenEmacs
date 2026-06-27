;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-workspace.el --- Local workspace + templates -*- lexical-binding: t; -*-

;;; Commentary:
;; Per-problem directories under `~/.emacs-codeforces/workspace/' and language
;; template instantiation.  Pure local-filesystem, no network.

;;; Code:

;; Declared as `defcustom' in emacs-codeforces.el; defvar here so this module
;; can be loaded and tested standalone.
(defvar codeforces-home-directory nil
  "Codeforces workspace root.  Set by `emacs-codeforces.el'.")

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

(defun +cf-init-solution (problem language)
  "Create PROBLEM's workspace dir and init solution.<ext> from LANGUAGE template.
Returns the path to the solution file.  Never overwrites an existing solution."
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
    sol))

(provide 'emacs-codeforces-workspace)

;;; emacs-codeforces-workspace.el ends here
