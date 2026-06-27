;;; init-org.el --- Org configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Org authoring, agenda, capture, reporting, export, and transclusion setup.

;;; Code:

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . auto-fill-mode))
  :config
  (setq org-startup-indented t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-use-speed-commands t
        org-directory (rivenEmacs-get-org-directory)
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-archive-location (expand-file-name "archive/%s::" org-directory)
        org-startup-with-inline-images t
        org-startup-with-latex-preview t
        org-latex-create-formula-image-program 'latexmk
        org-latex-compiler "xelatex"
        org-latex-listings 'listings
        org-latex-packages-alist '(("" "amsmath" t)
                                   ("" "amssymb" t)
                                   ("" "amsfonts" t)
                                   ("" "graphicx" t)
                                   ("" "xcolor" t))
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate t
        org-babel-python-command "python3"
        org-todo-keywords
        '((sequence
           "TODO(t!)"
           "PROJ(p!)"
           "NEXT(n!)"
           "WAIT(w@)"
           "|"
           "DONE(d!)"
           "KILL(k@)")
          (sequence
           "[ ](T)"
           "[-](S)"
           "[?](W)"
           "|"
           "[X](D)"))
        org-todo-keyword-faces
        '(("TODO" :foreground "red1" :weight bold)
          ("PROJ" :foreground "deep sky blue" :weight bold)
          ("NEXT" :foreground "dark orange" :weight bold)
          ("WAIT" :foreground "magenta1" :weight bold :background "gray15")
          ("DONE" :foreground "forest green" :weight bold)
          ("KILL" :foreground "dim gray" :weight bold :strike-through t)
          ("[ ]" :foreground "red1" :weight bold)
          ("[-]" :foreground "dark orange" :weight bold)
          ("[?]" :foreground "magenta1" :weight bold)
          ("[X]" :foreground "forest green" :weight bold))
        org-clock-persist 'history
        org-clock-out-when-done t
        org-clock-report-include-clocking-task t
        org-auto-clock-resolution 'when-no-clock
        org-clock-idle-time 15
        org-clock-modeline-total 'current)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (js . t)
     (C . t)
     (latex . t)
     (haskell . t)
     (sql . t)
     (R . t)
     (matlab . t)))
  (org-clock-persistence-insinuate))

(defun riven/org-agenda-show-calendar (&rest _)
  "Show and mark calendar holidays after opening `org-agenda'."
  (when (equal (buffer-name) "*Org Agenda*")
    (calendar)
    (calendar-mark-holidays)
    (other-window 1)))

(defun riven/org-agenda-quit-calendar ()
  "Close the calendar window when quitting `org-agenda'."
  (let ((window (get-buffer-window calendar-buffer)))
    (when (and window (not (one-window-p window)))
      (delete-window window))))

(use-package org-agenda
  :ensure org
  :after org
  :config
  (unless (file-directory-p org-directory)
    (make-directory org-directory t))
  (setq org-agenda-files (list (expand-file-name "Agenda" org-directory))
        org-agenda-start-on-weekday nil
        org-agenda-span 'day
        org-agenda-deadline-faces '((1.0 . org-warning)
                                    (0.0 . org-error))
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-tags-column -105
        org-agenda-sticky t
        org-agenda-time-grid '((daily today require-timed)
                               (300 600 900 1200 1500 1800 2100 2400)
                               "......"
                               "-----------------------------------------------------"))
  (advice-add 'org-agenda :after #'riven/org-agenda-show-calendar)
  (advice-add 'org-agenda-quit :before #'riven/org-agenda-quit-calendar))

(use-package org-capture
  :ensure org
  :after org
  :config
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %i\n  %a" :prepend t)
          ("j" "Journal" entry (file+datetree
                                (expand-file-name "journal.org" org-directory))
           "* %<%Y-%m-%d %a %H:%M> %?\n%i%a" :tree-type week :prepend t)
          ("n" "Note" entry (file+headline org-default-notes-file "Notes")
           "* %? :NOTE:\n%i%a" :prepend t)
          ("p" "Project Idea" entry (file+headline org-default-notes-file "Projects")
           "* PROJ %? :PROJECT:\n%i%a" :prepend t))))

(setq org-level-color-stars-only nil
      org-fontify-whole-heading-line t
      org-cycle-level-faces nil
      org-n-level-faces 8)

(use-package org-ql
  :ensure t
  :after org)

(require 'ox-beamer-lecture)

(defun get-last-friday (&optional from-time)
  "Return the most recent Friday before or at FROM-TIME."
  (let* ((from-time (or from-time (ts-adjust 'day -1 (ts-now))))
         (adjust-prev-friday (- (mod (- (ts-dow from-time) 5) 7))))
    (ts-adjust 'day adjust-prev-friday from-time)))

(defun report-last-week-tasks ()
  "Search agenda files for tasks planned during the last Friday-based week."
  (interactive)
  (let ((query
         `(and
           (planning :from ,(ts-adjust 'day -7 (get-last-friday)))
           (planning :to ,(ts-adjust 'day -1 (get-last-friday))))))
    (org-ql-search org-agenda-files query)))


(use-package org-transclusion
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook #'org-transclusion-mode)
  (setq org-transclusion-default-tc-type "org"
        org-transclusion-add-replace-id-when-create t
        org-transclusion-sync-at-saving t
        org-transclusion-face 'org-block))

;;; Beautification (org-appear + org-modern + olivetti)

;; Reveal hidden Org markers (emphasis, links, sub/superscript) only while
;; point is on them. Pairs with `org-hide-emphasis-markers' set above.
(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t))

;; Modern Org styling: replaces org-superstar/org-bullets. Uses display text
;; properties (future-proof, performant). Keeps the per-level face sizes set
;; above.
;;
;; NOTE: deliberately leave `org-modern-star' at its default ('fold) rather
;; than overriding with Unicode bullets. CJK fonts render arbitrary Unicode
;; symbols (◉ ✸ ✿ ◆ ▶) at inconsistent sizes/widths, which looked broken.
;; The default fold style (hide leading stars, indent by level) is clean and
;; consistent across all scripts.
(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  ;; Disable org-modern's tag styling. When `org-modern-tag' is non-nil (the
  ;; default) it wraps tags in the `org-modern-label' face and inserts display
  ;; properties around the colon separators, which renders with mismatched font
  ;; sizes — a big colon followed by tiny tag text. With it disabled, Org falls
  ;; back to its native `org-tag' face (:weight bold), where the colon and the
  ;; tag text share one consistent font/size.
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka")
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-agenda-tags-column 0
   org-ellipsis "…")
  (customize-set-variable 'org-modern-tag nil))

;; Centered writing with a soft text width. `ews-olivetti' in ews.el provides
;; the distraction-free toggle; this just makes olivetti available + a sane
;; default width for Org buffers.
(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 100)
  :hook (org-mode . olivetti-mode))

(provide 'init-org)
;;; init-org.el ends here
