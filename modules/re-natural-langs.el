;;; re-natural-langs.el --- Natural languages stuff -*- lexical-binding: t; -*-

(defconst +aspell-available-p (executable-find "aspell"))

(use-package spell-fu
  :straight t
  :when +aspell-available-p
  :hook (text-mode . spell-fu-mode)
  :custom
  (spell-fu-directory (+directory-ensure rivenemacs-local-dir "spell-fu/"))
  :init
  (+map! "ts" #'spell-fu-mode)
  (+nvmap! "z=" #'+spell-fu-correct) ;; autoloaded from "re-spell-fu.el"

  (defcustom +spell-excluded-faces-alist
    '((markdown-mode
       . (markdown-code-face
          markdown-html-attr-name-face
          markdown-html-attr-value-face
          markdown-html-tag-name-face
          markdown-inline-code-face
          markdown-link-face
          markdown-markup-face
          markdown-plain-url-face
          markdown-reference-face
          markdown-url-face))
      (org-mode
       . (org-block
          org-block-begin-line
          org-block-end-line
          org-cite
          org-cite-key
          org-code
          org-date
          org-footnote
          org-formula
          org-inline-src-block
          org-latex-and-related
          org-link
          org-meta-line
          org-property-value
          org-ref-cite-face
          org-special-keyword
          org-tag
          org-todo
          org-todo-keyword-done
          org-todo-keyword-habt
          org-todo-keyword-kill
          org-todo-keyword-outd
          org-todo-keyword-todo
          org-todo-keyword-wait
          org-verbatim))
      (latex-mode
       . (font-latex-math-face
          font-latex-sedate-face
          font-lock-function-name-face
          font-lock-keyword-face
          font-lock-variable-name-face)))
    "Faces in certain major modes that spell-fu will not spellcheck."
    :group 'rivenemacs-ui
    :type '(repeat (cons symbol (repeat face))))

  (add-hook
   'spell-fu-mode-hook
   (defun +spell-fu--init-excluded-faces-h ()
     "Set `spell-fu-faces-exclude' according to `+spell-excluded-faces-alist'."
     (when-let (excluded (cdr (cl-find-if #'derived-mode-p +spell-excluded-faces-alist :key #'car)))
       (setq-local spell-fu-faces-exclude excluded)))))

(use-package lexic
  :straight t
  :preface
  (defconst +sdcv-available-p (executable-find "sdcv"))
  :when +sdcv-available-p
  :init
  (+map! :infix "s"
    "l" #'lexic-search-word-at-point
    "L" #'lexic-search)
  :config
  (+map-local! :keymaps 'lexic-mode-map
    "q" #'lexic-return-from-lexic
    "RET" #'lexic-search-word-at-point
    "a" #'outline-show-all
    "h" `(,(+cmdfy! (outline-hide-sublevels 3)) :wk "Hide sublevels")
    "o" #'lexic-toggle-entry
    "n" #'lexic-next-entry
    "N" `(,(+cmdfy! (lexic-next-entry t)) :wk "Last entry")
    "p" #'lexic-previous-entry
    "P" `(,(+cmdfy! (lexic-previous-entry t)) :wk "First entry")
    "E" `(,(+cmdfy!
            (lexic-return-from-lexic)
            (switch-to-buffer (lexic-get-buffer)))
          :wk "Expand")
    "M" `(,(+cmdfy!
            (lexic-return-from-lexic)
            (lexic-goto-lexic))
          :wk "Minimise")
    "C-p" #'lexic-search-history-backwards
    "C-n" #'lexic-search-history-forwards
    "/" `(,(+cmdfy! (call-interactively #'lexic-search)) :wk "Search")))

(provide 're-natural-langs)
