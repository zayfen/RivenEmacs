;; init-org.el
;;
;; Comprehensive Org Mode configuration for Emacs.
;; To use this file:
;; 1. Save it as `init-org.el` (e.g., in `~/.emacs.d/lisp/init-org.el`).
;; 2. In your main `init.el` (or `~/.emacs`), add:
;;    (add-to-list 'load-path "~/.emacs.d/lisp/") ; Or wherever you save it
;;    (require 'init-org)
;;
;; Make sure you have package.el set up in your main init.el before loading this.

;; --------------------------------------------------------------------------
;; SECTION 1: CORE ORG MODE CONFIGURATION
;; --------------------------------------------------------------------------
(use-package org
  ;; :ensure t ; Uncomment if you want to ensure the latest version from ELPA.
  ;; Org is built-in, but this allows overriding with a newer version if desired.
  ;; :pin org  ; Pin to the org-mode.org ELPA archive if you use it.
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode) ; Enable visual line mode for better readability
         (org-mode . auto-fill-mode))   ; Enable auto-fill mode for text

  :init
  (message "Initializing Org Mode core settings...")

  :config
  ;; --- General Org Behavior ---
  (setq org-startup-indented t)             ; Nicer indentation for outlines
  (setq org-ellipsis " ▼ ")                ; Prettier ellipsis character
  (setq org-hide-emphasis-markers t)      ; Hide *, /, _, etc., around emphasized text
  (setq org-pretty-entities t)            ; Display LaTeX-like entities (e.g., \alpha as α)
  (setq org-startup-folded 'content)      ; Fold all but the top-level headlines
  ;; (setq org-startup-folded 'overview)  ; Or just show headlines
  (setq org-cycle-separator-lines 2)      ; More space when cycling visibility
  (setq org-use-speed-commands t)         ; Enable single-key commands on headlines

  ;; --- File Handling ---
  (setq org-directory (rivenEmacs-get-org-directory))             ; Default directory for Org files
  (setq org-default-notes-file (concat org-directory "/notes.org")) ; Default capture file
  (setq org-archive-location (concat org-directory "/archive/%s::")) ; Archive location

  ;; --- Image Display ---
  ;; Automatically display inline images when opening an Org file.
  (setq org-startup-with-inline-images t)
  ;; You might need to set `image-actual-display' to t if images don't show.
  ;; (setq image-actual-display t)
  ;; Control image width (e.g., max 500 pixels)
  ;; (setq org-image-actual-width 500)

  ;; --- Math (LaTeX Preview) Display ---
  ;; Prerequisites: A LaTeX distribution (TeX Live, MiKTeX, etc.) and `dvipng` or `dvisvgm`.
  (setq org-startup-with-latex-preview t)
  (setq org-latex-create-formula-image-program 'dvipng) ; Common choice, creates PNGs
  ;; (setq org-latex-create-formula-image-program 'dvisvgm) ; Creates SVGs, often sharper
  (setq org-latex-listings 'listings) ; For code listings in LaTeX export
  (setq org-latex-packages-alist '(("" "amsmath" t)
                                   ("" "amssymb" t)
                                   ("" "amsfonts" t)
                                   ("" "graphicx" t)
                                   ("" "xcolor" t))) ; Common LaTeX packages

  ;; --- Code Block Enhancements ---
  (setq org-src-fontify-natively t)          ; Use native font-locking from language major modes
  (setq org-src-tab-acts-natively t)        ; TAB key behaves as in the language's major mode
  (setq org-src-window-setup 'current-window) ; Where to display results of code execution
  (setq org-confirm-babel-evaluate t)     ; Set to 't to prompt before executing code blocks
  (setq org-babel-python-command "python3")
  ;; Enable languages for Org Babel (add more as needed)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (js . t)
     (C . t)
     ;;(cpp . t)
     (latex . t)
     (haskell . t)
     ;; (org . t) ; For evaluating org code blocks within org
     (sql . t)
     (R . t)
     (matlab . t)
     ))

  ;; --- TODO Keywords and Appearance ---
  (setq org-todo-keywords
        '((sequence
           "TODO(t!)"  ; A task that needs doing (with note prompt)
           "PROJ(p!)"  ; A project
           "NEXT(n!)"  ; Next action
           "WAIT(w@)"  ; Waiting for something/someone (with note prompt on state change)
           "|"         ; Separator for done states
           "DONE(d!)"  ; Task completed (with note prompt)
           "KILL(k@)") ; Task cancelled (with note prompt on state change)
          (sequence
           "[ ](T)"    ; Simple checkbox todo
           "[-](S)"    ; In-progress checkbox
           "[?](W)"    ; Question/hold checkbox
           "|"
           "[X](D)")   ; Done checkbox
          ))

  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red1" :weight 'bold)
          ("PROJ" :foreground "deep sky blue" :weight 'bold)
          ("NEXT" :foreground "dark orange" :weight 'bold)
          ("WAIT" :foreground "magenta1" :weight 'bold :background "gray15")
          ("DONE" :foreground "forest green" :weight 'bold)
          ("KILL" :foreground "dim gray" :weight 'bold :strike-through t)
          ("[ ]"  :foreground "red1" :weight 'bold)
          ("[-]"  :foreground "dark orange" :weight 'bold)
          ("[?]"  :foreground "magenta1" :weight 'bold)
          ("[X]"  :foreground "forest green" :weight 'bold)
          ))

  ;; --- Clocking Configuration ---
  (setq org-clock-persist 'history)             ; Save clock history across Emacs sessions
  (org-clock-persistence-insinuate)             ; Load clock history when Org mode starts
  (setq org-clock-out-when-done t)              ; Automatically clock out when marking a task DONE
  (setq org-clock-report-include-clocking-task t) ; Show current clocking task in report
  (setq org-auto-clock-resolution 'when-no-clock) ; Auto clock in when starting tasks if nothing else is clocked
  (setq org-clock-idle-time 15)                 ; Ask what to do after 15 minutes of idle time while clocked in

  ;; --- Modeline Display for Clocked Task ---
  ;; Display total time for the currently clocked task in the modeline.
  ;; The standard Org modeline already shows the current headline.
  (setq org-clock-modeline-total 'current) ; Show time for current task, or 'today for all today
  ;; To customize the format further, you might explore `org-mode-line-clock-format`
  ;; or packages like `doom-modeline` for more advanced modeline displays.

  (setq org-latex-create-formula-image-program 'latexmk)
  (setq org-latex-compiler "xelatex")
  (message "Org Mode core configuration loaded.")

  )

;; --------------------------------------------------------------------------
;; SECTION 2: ORG AGENDA CONFIGURATION
;; --------------------------------------------------------------------------
(use-package org-agenda
  :ensure org ; Part of the org package
  :after org  ; Ensure org is loaded first
  :config
  (message "Configuring Org Agenda...")

  ;; Define the list of files to be included in the agenda.
  ;; Create the org-directory if it doesn't exist.
  (unless (file-directory-p org-directory)
    (make-directory org-directory t))

  (setq org-agenda-files (list (concat org-directory "/Agenda") ; Include all .org files in org-directory/Agenda
                               ;; You can add specific files too:
                               ;; (concat org-directory "/work.org")
                               ;; (concat org-directory "/personal.org")
                               ))

  ;; Customize agenda appearance and behavior
  (setq org-agenda-start-on-weekday nil)      ; Start agenda view on current day
  (setq org-agenda-span 'day)                 ; Default agenda span (day, week, month, year)
  (setq org-agenda-deadline-faces
        '((1.0 . org-warning)                 ; Due today
          (0.0 . org-error)))                 ; Overdue
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-tags-column -105)          ; Adjust width for tags column
  (setq org-agenda-sticky t)                  ; Keep agenda buffer open

  ;;---------------------------------------------
  ;;org-agenda-time-grid
  ;;--------------------------------------------
  (setq org-agenda-time-grid (quote ((daily today require-timed)
                                     (300
                                      600
                                      900
                                      1200
                                      1500
                                      1800
                                      2100
                                      2400)
                                     "......"
                                     "-----------------------------------------------------"
                                     )))

  ;;integrated with Calendar
  (general-advice-add 'org-agenda :after
                      (lambda (_)
                        (when (equal (buffer-name)
                                     "*Org Agenda*")
                          (calendar)
                          (calendar-mark-holidays)
                          (other-window 1))))

  (general-advice-add 'org-agenda-quit :before
                      (lambda ()
                        (let ((window (get-buffer-window calendar-buffer)))
                          (when (and window (not (one-window-p window)))
                            (delete-window window)))))


  ;; ;; Keybindings for agenda (global, easily accessible)
  ;; (global-set-key (kbd "C-c a") 'org-agenda)  ; Main agenda dispatcher
  ;; (global-set-key (kbd "C-c c") 'org-capture) ; Capture new items

  (message "Org Agenda configuration loaded.")
  )

;; --------------------------------------------------------------------------
;; SECTION 3: ORG CAPTURE TEMPLATES (EXAMPLE)
;; --------------------------------------------------------------------------
(use-package org-capture
  :ensure org ; Part of the org package
  :after org
  :config
  (message "Configuring Org Capture...")
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %i\n  %a" :prepend t)
          ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
           "* %<%Y-%m-%d %a %H:%M> %?\n%i%a" :tree-type week :prepend t)
          ("n" "Note" entry (file+headline org-default-notes-file "Notes")
           "* %? :NOTE:\n%i%a" :prepend t)
          ("p" "Project Idea" entry (file+headline org-default-notes-file "Projects")
           "* PROJ %? :PROJECT:\n%i%a" :prepend t)
          ))
  (message "Org Capture templates configured.")
  )


(setq org-level-color-stars-only nil
      org-fontify-whole-heading-line t
      org-cycle-level-faces nil
      org-n-level-faces 8)

(use-package org-ql
  :ensure t
  :after org)

;; install ox-beamer-lecture https://github.com/fjesser/ox-beamer-lecture.git
(require 'ox-beamer-lecture)

(defun get-last-friday (&optional from-time)
  (let* ((from-time (or from-time (ts-adjust 'day -1 (ts-now))))
         (adjust-prev-friday (- (mod (- (ts-dow from-time) 5) 7))))
    (ts-adjust 'day adjust-prev-friday from-time)))

(defun report-last-week-tasks ()
  (interactive)
  (let ((query
         `(and
             (planning :from ,(ts-adjust 'day -7 (get-last-friday)))
             (planning :to ,(ts-adjust 'day -1 (get-last-friday))))))
    (org-ql-search org-agenda-files query)))

;; This makes each level 10% smaller than the previous
(custom-set-faces
 '(org-level-1 ((t (:height 1.5 :weight bold))))
 '(org-level-2 ((t (:height 1.4 :weight bold))))
 '(org-level-3 ((t (:height 1.3 :weight bold))))
 '(org-level-4 ((t (:height 1.2 :weight bold))))
 '(org-level-5 ((t (:height 1.1 :weight bold))))
 '(org-level-6 ((t (:height 1.0 :weight bold)))))

;; --------------------------------------------------------------------------
(provide 'init-org)
;;; init-org.el ends here
