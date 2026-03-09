;;; keybindings-engine.el --- Declarative keybinding engine -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'keybindings-spec)

(defvar riven/keybindings--seen (make-hash-table :test #'equal)
  "Internal table for duplicate key detection.")

(defun riven/keybindings--symbol-name-safe (sym)
  (if (symbolp sym) (symbol-name sym) (format "%s" sym)))

(defun riven/keybindings--register (namespace key cmd)
  "Register KEY/CMD in NAMESPACE and warn on duplicates."
  (let* ((id (format "%s:%s" namespace key))
         (prev (gethash id riven/keybindings--seen)))
    (when (and prev (not (equal prev cmd)))
      (message "[keybindings] duplicate key %s (%s -> %s)"
               id
               (riven/keybindings--symbol-name-safe prev)
               (riven/keybindings--symbol-name-safe cmd)))
    (puthash id cmd riven/keybindings--seen)))

(defun riven/keybindings--warn-missing-command (cmd)
  "Warn when CMD is not callable."
  (unless (or (keywordp cmd)
              (and (symbolp cmd) (fboundp cmd)))
    (message "[keybindings] missing command: %s" (riven/keybindings--symbol-name-safe cmd))))

(defun riven/keybindings--bind-and-describe (full-key cmd wk)
  "Bind FULL-KEY to CMD in global map and register WK description."
  (keymap-global-set full-key cmd)
  (when (and wk (fboundp 'which-key-add-key-based-replacements))
    (which-key-add-key-based-replacements full-key wk)))

(defun riven/keybindings-apply-leader-spec ()
  "Apply leader groups from declarative spec using C-c prefix."
  (dolist (group riven/keybindings-leader-spec)
    (pcase-let ((`(,prefix ,title ,bindings) group))
      (when (fboundp 'which-key-add-key-based-replacements)
        (which-key-add-key-based-replacements (concat "C-c " prefix) title))
      (dolist (entry bindings)
        (pcase-let ((`(,key ,cmd ,wk) entry))
          (riven/keybindings--register (format "leader-%s" prefix) key cmd)
          (riven/keybindings--warn-missing-command cmd)
          (riven/keybindings--bind-and-describe
           (concat "C-c " prefix " " key) cmd wk))))))

(defun riven/keybindings-apply-simple-spec (prefix namespace title spec)
  "Bind SPEC keys under C-c PREFIX, registering TITLE and descriptions."
  (when (fboundp 'which-key-add-key-based-replacements)
    (which-key-add-key-based-replacements (concat "C-c " prefix) title))
  (dolist (entry spec)
    (pcase-let ((`(,key ,cmd ,wk) entry))
      (riven/keybindings--register namespace key cmd)
      (riven/keybindings--warn-missing-command cmd)
      (riven/keybindings--bind-and-describe
       (concat "C-c " prefix " " key) cmd wk))))

(defun riven/keybindings-apply-open-query-ai ()
  "Apply open/query/ai specs."
  (riven/keybindings-apply-simple-spec "o" "open" "Open" riven/keybindings-open-spec)
  (riven/keybindings-apply-simple-spec "q" "query" "Query" riven/keybindings-query-spec)
  (riven/keybindings-apply-simple-spec "a" "ai" "AI" riven/keybindings-ai-spec)
  (when (fboundp 'agent-shell)
    (when (fboundp 'which-key-add-key-based-replacements)
      (which-key-add-key-based-replacements "C-c =" "Agent"))
    (dolist (entry riven/keybindings-agent-spec)
      (pcase-let ((`(,key ,cmd ,wk) entry))
        (riven/keybindings--register "agent" key cmd)
        (riven/keybindings--warn-missing-command cmd)
        (riven/keybindings--bind-and-describe (concat "C-c = " key) cmd wk)))))

(defun riven/keybindings-apply-navigate ()
  "Apply M-g navigation spec."
  (when (fboundp 'which-key-add-key-based-replacements)
    (which-key-add-key-based-replacements "M-g" "Navigate"))
  (dolist (entry riven/keybindings-navigate-spec)
    (pcase-let ((`(,key ,cmd ,wk) entry))
      (riven/keybindings--register "navigate" key cmd)
      (riven/keybindings--warn-missing-command cmd)
      (keymap-global-set (concat "M-g " key) cmd)
      (when (and wk (fboundp 'which-key-add-key-based-replacements))
        (which-key-add-key-based-replacements (concat "M-g " key) wk)))))

(defun riven/keybindings-apply-default-cleanups ()
  "Apply one-time key cleanup."
  (keymap-global-unset "M-g TAB")
  (keymap-global-unset "M-g M-g"))

(provide 'keybindings-engine)
