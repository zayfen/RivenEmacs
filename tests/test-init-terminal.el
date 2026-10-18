;;; test-init-terminal.el --- Tests for init-terminal -*- lexical-binding: t; -*-

(require 'ert)
(require 'keybindings-spec-core)

(defconst rivenEmacs-terminal-test-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the RivenEmacs repository.")

(defun rivenEmacs-terminal-test-read-forms (file)
  "Read all top-level Lisp forms from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let (forms)
      (condition-case nil
          (while t
            (push (read (current-buffer)) forms))
        (end-of-file (nreverse forms))))))

(defun rivenEmacs-terminal-test-use-package-names ()
  "Return package names declared by `use-package' in init-terminal."
  (let ((file (expand-file-name "lisp/tools/init-terminal.el" rivenEmacs-terminal-test-root)))
    (delq nil
          (mapcar (lambda (form)
                    (when (and (consp form) (eq (car form) 'use-package))
                      (cadr form)))
                  (rivenEmacs-terminal-test-read-forms file)))))

(ert-deftest rivenEmacs-terminal-test-declares-ghostel-and-fallbacks ()
  "The terminal module declares ghostel as its main terminal plus fallbacks."
  (let ((packages (rivenEmacs-terminal-test-use-package-names)))
    (should (member 'ghostel packages))
    (should (member 'eat packages))
    (should (member 'vterm packages))))

(ert-deftest rivenEmacs-terminal-test-open-keybinding-uses-ghostel ()
  "C-c t t is bound to ghostel by the declarative Tool group spec."
  (let ((tool-group (assoc "t" riven/keybindings-leader-spec-core)))
    (should tool-group)
    (should (member '("t" ghostel "Terminal") (nth 2 tool-group)))))

(ert-deftest rivenEmacs-terminal-test-ghostel-cjk-font ()
  "Ghostel selects a terminal CJK font and disables glyph shrinking."
  (let* ((root rivenEmacs-terminal-test-root)
         (terminal (with-temp-buffer
                     (insert-file-contents
                      (expand-file-name "lisp/tools/init-terminal.el" root))
                     (buffer-string)))
         (font (with-temp-buffer
                 (insert-file-contents
                  (expand-file-name "lisp/ui/init-font.el" root))
                 (buffer-string))))
    (should (string-match-p "(defcustom rivenEmacs-ghostel-font-family" terminal))
    (should (string-match-p "(defun riven/ghostel-configure-font" terminal))
    (should (string-match-p "ghostel-mode-hook #'riven/ghostel--mode-setup" terminal))
    (should (string-match-p "Sarasa Term SC Nerd" font))))

;;; test-init-terminal.el ends here
