;;; test-init-markdown.el --- Tests for Markdown rendering integration -*- lexical-binding: t; -*-

;;; Commentary:
;; `markdown-ts-appear' is wired to the built-in `markdown-ts-mode'; keep
;; the dispatch, grammar sources, rich `C-c l' map, and package hook covered.

;;; Code:

(require 'ert)

(defconst riven/test-init-markdown-repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the RivenEmacs repository.")

(defun riven/test-init-markdown-file (relative-path)
  "Return the contents of RELATIVE-PATH in the repository."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name relative-path riven/test-init-markdown-repo-root))
    (buffer-string)))

(ert-deftest riven/markdown-ts-appear-is-wired ()
  "Markdown opens in `markdown-ts-mode' when grammars are ready."
  (let ((config (riven/test-init-markdown-file "lisp/editor/init-markdown.el")))
    (should (string-match-p "(use-package markdown-ts-appear" config))
    (should (string-match-p "markdown-ts-appear-mode" config))
    (should (string-match-p "riven/markdown-maybe-ts-mode" config))
    (should (string-match-p "riven/markdown-install-ts-keys" config))))

(ert-deftest riven/markdown-grammars-are-configured ()
  "The managed tree-sitter list includes both Markdown grammars."
  (let ((treesit-config
         (riven/test-init-markdown-file "lisp/parser/init-treesit.el")))
    (should (string-match-p "(markdown ." treesit-config))
    (should (string-match-p "(markdown-inline ." treesit-config))
    (should (string-match-p "markdown markdown-inline" treesit-config))))

;;; test-init-markdown.el ends here
