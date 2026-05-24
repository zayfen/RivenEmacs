;;; test-structured-keybindings.el --- Structured keybinding tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'init-keybindings)

(defmacro riven/test-with-quiet-keybinding-diagnostics (&rest body)
  "Evaluate BODY without missing-command diagnostics from unloaded packages."
  `(cl-letf (((symbol-function 'riven/keybindings--warn-missing-command) #'ignore))
     ,@body))

(defun riven/test-apply-structured-keybindings ()
  "Apply keybindings in a quiet test context."
  (riven/test-with-quiet-keybinding-diagnostics
   (riven/keybindings-config)))

(ert-deftest riven/structured-keybindings-file-group ()
  "File group owns file and file-backed buffer commands."
  (riven/test-apply-structured-keybindings)
  (dolist (entry '(("C-c f f" . consult-fd)
                   ("C-c f F" . +consult-fd-in-home)
                   ("C-c f r" . consult-recent-file)
                   ("C-c f d" . crux-recentf-find-directory)
                   ("C-c f s" . deadgrep)
                   ("C-c f g" . consult-git-grep)
                   ("C-c f b" . ibuffer-list-buffers)
                   ("C-c f p" . riven/keybindings-copy-current-file-path)
                   ("C-c f n" . riven/keybindings-rename-current-file-or-buffer)
                   ("C-c f D" . riven/keybindings-delete-current-file-or-buffer)
                   ("C-c f R" . revert-buffer-quick)
                   ("C-c f e" . set-buffer-file-coding-system)
                   ("C-c f m" . bookmark-set)
                   ("C-c f h" . vundo)))
    (should (eq (key-binding (kbd (car entry))) (cdr entry)))))

(ert-deftest riven/structured-keybindings-project-search-and-session-groups ()
  "Project, Search, and Session groups use noun prefixes."
  (riven/test-apply-structured-keybindings)
  (dolist (entry '(("C-c p p" . project-switch-project)
                   ("C-c p f" . project-find-file)
                   ("C-c p s" . consult-ripgrep-ex)
                   ("C-c p d" . riven/keybindings-project-dired)
                   ("C-c s l" . consult-line)
                   ("C-c s i" . consult-imenu)
                   ("C-c s o" . consult-outline)
                   ("C-c s c" . avy-goto-char-2)
                   ("C-c s /" . consult-ripgrep)
                   ("C-c x s" . rivenEmacs-session-save)
                   ("C-c x l" . rivenEmacs-session-load)
                   ("C-c x d" . rivenEmacs-session-delete)))
    (should (eq (key-binding (kbd (car entry))) (cdr entry)))))

(ert-deftest riven/structured-keybindings-code-error-tool-window-ai-agent-groups ()
  "Remaining top-level groups expose representative approved commands."
  (riven/test-apply-structured-keybindings)
  (dolist (entry '(("C-c g g" . magit-status)
                   ("C-c g n" . diff-hl-next-hunk)
                   ("C-c g p" . diff-hl-previous-hunk)
                   ("C-c c d" . riven/xref-find-definitions-or-search)
                   ("C-c c D" . riven/eglot-find-declaration-dispatch)
                   ("C-c c a" . eglot-code-actions)
                   ("C-c c s" . sp-splice-sexp)
                   ("C-c c q" . quickrun)
                   ("C-c e l" . riven/flymake-show-buffer-diagnostics-focus)
                   ("C-c e n" . flymake-goto-next-error)
                   ("C-c t t" . eat)
                   ("C-c t d" . docker)
                   ("C-c t ." . fanyi-dwim)
                   ("C-c w s" . split-window-below)
                   ("C-c w u" . winner-undo)
                   ("C-c a a" . ai-code-menu)
                   ("C-c a c" . gptel)
                   ("C-c a S" . riven/gptel-summarize-region-review)
                   ("C-c = =" . agent-shell)))
    (should (eq (key-binding (kbd (car entry))) (cdr entry)))))

(ert-deftest riven/structured-keybindings-retire-old-semantic-prefixes ()
  "Old Buffer/Open/Query/Session meanings are not kept as aliases."
  (riven/test-apply-structured-keybindings)
  (dolist (entry '(("C-c b l" . ibuffer-list-buffers)
                   ("C-c o t" . eat)
                   ("C-c o d" . docker)
                   ("C-c q d" . devdocs-lookup)
                   ("C-c q ." . fanyi-dwim)
                   ("C-c s s" . rivenEmacs-session-save)
                   ("C-c s d" . rivenEmacs-session-delete)))
    (should-not (eq (key-binding (kbd (car entry))) (cdr entry)))))

(ert-deftest riven/structured-keybindings-clear-stale-owned-prefixes ()
  "Reapplying keybindings clears stale commands from retired owned prefixes."
  (dolist (entry '(("C-c b l" . ibuffer-list-buffers)
                   ("C-c o t" . eat)
                   ("C-c q d" . devdocs-lookup)
                   ("C-c s d" . rivenEmacs-session-delete)
                   ("C-c ! l" . flymake-show-buffer-diagnostics)
                   ("C-c e u" . sp-splice-sexp)))
    (keymap-global-set (car entry) (cdr entry)))
  (riven/test-apply-structured-keybindings)
  (dolist (entry '(("C-c b l" . ibuffer-list-buffers)
                   ("C-c o t" . eat)
                   ("C-c q d" . devdocs-lookup)
                   ("C-c s d" . rivenEmacs-session-delete)
                   ("C-c ! l" . flymake-show-buffer-diagnostics)
                   ("C-c e u" . sp-splice-sexp)))
    (should-not (eq (key-binding (kbd (car entry))) (cdr entry)))))

(ert-deftest riven/structured-keybinding-helper-commands-are-interactive ()
  "Helper commands used by specs exist and are interactive."
  (dolist (command '(riven/keybindings-copy-current-file-path
                     riven/keybindings-rename-current-file-or-buffer
                     riven/keybindings-delete-current-file-or-buffer
                     riven/keybindings-project-dired
                     riven/keybindings-symbol-overlay-dispatch))
    (should (fboundp command))
    (should (commandp command))))

(ert-deftest riven/structured-keybinding-spec-has-no-conflicting-duplicates ()
  "Declarative leader specs do not contain conflicting duplicate group actions."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (group riven/keybindings-leader-spec)
      (pcase-let ((`(,prefix ,_title ,bindings) group))
        (dolist (entry bindings)
          (pcase-let* ((`(,key ,cmd ,_wk) entry)
                       (id (format "%s:%s" prefix key))
                       (previous (gethash id seen)))
            (when previous
              (should (eq previous cmd)))
            (puthash id cmd seen)))))
    (should (= (hash-table-count seen)
               (apply #'+
                      (mapcar (lambda (group) (length (nth 2 group)))
                              riven/keybindings-leader-spec))))))

(provide 'test-structured-keybindings)
;;; test-structured-keybindings.el ends here
