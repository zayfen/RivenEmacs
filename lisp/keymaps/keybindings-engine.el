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

(defun riven/keybindings-invoke-definer (definer args)
  "Invoke DEFINER with ARGS, supporting both macros and functions."
  (cond
   ((and (symbolp definer) (macrop definer))
    ;; Macros receive forms: keep scalar args as-is, quote list payloads only.
    (eval (cons definer
                (mapcar (lambda (arg)
                          (if (listp arg)
                              (list 'quote arg)
                            arg))
                        args))))
   ((or (functionp definer)
        (and (symbolp definer) (fboundp definer)))
    (apply definer args))
   (t
    (message "[keybindings] invalid definer: %s" definer))))

(defun riven/keybindings-apply-simple-spec (definer namespace title spec)
  "Apply SPEC with DEFINER under NAMESPACE/TITLE."
  (riven/keybindings-invoke-definer
   definer
   (append (list "" `(:ignore t :wk ,title))
           (cl-mapcan
            (lambda (entry)
              (pcase-let ((`(,key ,cmd ,wk) entry))
                (riven/keybindings--register namespace key cmd)
                (riven/keybindings--warn-missing-command cmd)
                (list key `(,cmd :wk ,wk))))
            spec))))

(defun riven/keybindings-apply-leader-spec ()
  "Apply `leader-def` groups from declarative spec."
  (dolist (group riven/keybindings-leader-spec)
    (pcase-let ((`(,prefix ,title ,bindings) group))
      (riven/keybindings-invoke-definer
       'leader-def
       (append (list :infix prefix "" `(:ignore t :wk ,title))
               (cl-mapcan
                (lambda (entry)
                  (pcase-let ((`(,key ,cmd ,wk) entry))
                    (riven/keybindings--register (format "leader-%s" prefix) key cmd)
                    (riven/keybindings--warn-missing-command cmd)
                    (list key `(,cmd :wk ,wk))))
                bindings))))))

(defun riven/keybindings-apply-open-query-ai ()
  "Apply open/query/ai specs."
  (riven/keybindings-apply-simple-spec 'open-leader-def "open" "Open" riven/keybindings-open-spec)
  (riven/keybindings-apply-simple-spec 'query-leader-def "query" "Query" riven/keybindings-query-spec)
  (riven/keybindings-apply-simple-spec 'ai-leader-def "ai" "AI" riven/keybindings-ai-spec)

  (when (fboundp 'agent-shell-leader-def)
    (riven/keybindings-apply-simple-spec 'agent-shell-leader-def "agent" "Agent" riven/keybindings-agent-spec)))

(defun riven/keybindings-apply-navigate ()
  "Apply M-g navigation spec."
  (riven/keybindings-invoke-definer
   'navigate-leader-def
   (cl-mapcan
    (lambda (entry)
      (pcase-let ((`(,key ,cmd ,wk) entry))
        (riven/keybindings--register "navigate" key cmd)
        (riven/keybindings--warn-missing-command cmd)
        (list key `(,cmd :wk ,wk))))
    riven/keybindings-navigate-spec)))

(defun riven/keybindings-apply-default-cleanups ()
  "Apply one-time non-declarative key cleanup."
  (leader-def
    "\!" '(:ignore t :wk "Checker(Flymake)")
    "&" '(:ignore t :wk "Yasnippet")
    "@" '(:ignore t :wk "Hideshow"))
  (keymap-global-unset "M-g TAB")
  (keymap-global-unset "M-g M-g"))

(provide 'keybindings-engine)
