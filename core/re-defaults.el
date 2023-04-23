;; re-defaults.el --- RivenEmacs defaults for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;; Inhibit startup message in echo area the brutal way!
;; The `inhibit-startup-echo-area-message' variable is very restrictive, there
;; is only one unique way of setting it right!
;; See: reddit.com/r/emacs/comments/6e9o4o/comment/di8q1t5
(fset 'display-startup-echo-area-message #'ignore)

;;; Why use anything but UTF-8?
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(set-locale-environment "en_US.UTF-8")

;; Use UTF-16-LE in Windows, see: rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
(set-selection-coding-system (if os/win 'utf-16-le 'utf-8))

(setq
 ;; ====== Default directories for builtin packages ======
 backup-directory-alist (list (cons "." (+directory-ensure rivenemacs-local-dir "backup/")))
 auto-save-list-file-prefix (+directory-ensure rivenemacs-local-dir "auto-save/")
 abbrev-file-name (concat rivenemacs-local-dir "abbrev.el")
 project-list-file (concat rivenemacs-local-dir "project-list.el")
 tramp-backup-directory-alist backup-directory-alist
 tramp-auto-save-directory (concat rivenemacs-local-dir "tramp/auto-save/")
 tramp-persistency-file-name (concat rivenemacs-local-dir "tramp/persistency.el")
 url-configuration-directory (+directory-ensure rivenemacs-local-dir "url/")
 url-cookie-file (concat rivenemacs-local-dir "url/cookie.el")
 url-history-file (concat rivenemacs-local-dir "url/history.el")
 url-cache-directory (+directory-ensure rivenemacs-cache-dir "url/")
 save-place-file (concat rivenemacs-local-dir "save-place.el")
 savehist-file (concat rivenemacs-local-dir "savehist.el")
 org-id-locations-file (concat rivenemacs-cache-dir "org/id-locations.el")
 org-persist-directory (+directory-ensure rivenemacs-cache-dir "org/persist/")
 org-publish-timestamp-directory (+directory-ensure rivenemacs-cache-dir "org/publish/timestamps/")
 org-preview-latex-image-directory (+directory-ensure rivenemacs-cache-dir "org/preview/latex-image/")
 recentf-save-file (concat rivenemacs-local-dir "recentf-save.el")
 shared-game-score-directory (+directory-ensure rivenemacs-local-dir "shared-game-score/")
 type-break-file-name (concat rivenemacs-local-dir "type-break.el")
 bookmark-default-file (concat rivenemacs-local-dir "bookmark.el")
 ede-project-placeholder-cache-file (concat rivenemacs-local-dir "ede-projects.el")
 kkc-init-file-name (concat rivenemacs-local-dir "kkc-init-file.el")
 erc-dcc-get-default-directory (+directory-ensure rivenemacs-local-dir "erc/dcc/")
 erc-log-channels-directory (+directory-ensure rivenemacs-local-dir "erc/log-channels/")
 eshell-directory-name (+directory-ensure rivenemacs-local-dir "eshell/")
 eshell-history-file-name (concat rivenemacs-local-dir "eshell/history.el")
 eshell-last-dir-ring-file-name (concat rivenemacs-local-dir "eshell/last-dir-ring.el")
 eshell-aliases-file (concat rivenemacs-local-dir "eshell/aliases")
 eshell-rc-script (concat rivenemacs-local-dir "eshell/rc")
 eshell-login-script (concat rivenemacs-local-dir "eshell/login")
 calc-settings-file (concat rivenemacs-local-dir "calc-settings.el")
 auto-insert-directory (+directory-ensure rivenemacs-local-dir "auto-insert/")
 image-dired-dir (+directory-ensure rivenemacs-local-dir "image-dired/")
 image-dired-tags-db-file (concat rivenemacs-local-dir "image-dired/tags-db.el")
 image-dired-temp-rotate-image-file (concat rivenemacs-cache-dir "image-dired/temp-rotate-image")
 eudc-options-file (concat rivenemacs-local-dir "eudc-options.el")
 eww-bookmarks-directory (+directory-ensure rivenemacs-local-dir "eww/bookmarks/")
 shadow-info-file (concat rivenemacs-local-dir "shadow/info.el")
 shadow-todo-file (concat rivenemacs-local-dir "shadow/todo.el")
 semanticdb-default-system-save-directory (concat rivenemacs-local-dir "semantic/")
 desktop-dirname (+directory-ensure rivenemacs-local-dir "desktop/")
 desktop-path (list desktop-dirname)
 custom-theme-directory (concat rivenemacs-config-dir "themes/")

 ;; ====== Default behavior ======
 ;; Inhibit startup message
 inhibit-startup-message t
 ;; Do not ring
 ring-bell-function #'ignore
 ;; Set to non-nil to flash!
 visible-bell nil
 ;; Increase the large file threshold to 20 MiB
 large-file-warning-threshold (* 20 1024 1024)
 ;; Initial scratch message (will be overridden if "fortune" is installed)
 initial-scratch-message ";; RivenEmacs -- start here!"
 ;; Set initial buffer to fundamental-mode for faster load
 initial-major-mode 'fundamental-mode
 ;; Always prompt in minibuffer (no GUI)
 use-dialog-box nil
 ;; Use y or n instead of yes or no
 use-short-answers t
 ;; Confirm before quitting
 confirm-kill-emacs #'y-or-n-p
 ;; Filter duplicate entries in kill ring
 kill-do-not-save-duplicates t
 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t
 ;; Save files only in sub-directories of current project
 save-some-buffers-default-predicate #'save-some-buffers-root
 ;; Use single space between sentences
 sentence-end-double-space nil
 ;; Move stuff to trash
 delete-by-moving-to-trash t
 ;; Select help window for faster quit!
 help-window-select t
 ;; More info on completions
 completions-detailed t
 ;; Do not ask obvious questions, follow symlinks
 vc-follow-symlinks t
 ;; Display the true file name for symlinks
 find-file-visit-truename t
 ;; Use completion in the minibuffer instead of definitions buffer
 xref-show-definitions-function #'xref-show-definitions-completing-read
 ;; Enable recursive calls to minibuffer
 enable-recursive-minibuffers t
 ;; Kill the shell buffer after exit
 shell-kill-buffer-on-exit t

 ;; ====== Passwords and encryption ======
 ;; Enable password caching
 password-cache t
 ;; One minute, default is 16
 password-cache-expiry 60
 ;; Default auth-sources to GPG
 auth-sources '("~/.authinfo.gpg")
 ;; Enable caching, do not keep asking about GPG key
 auth-source-do-cache t
 ;; All day, default is 2h (7200)
 auth-source-cache-expiry 86400

 ;; ====== Performances ======
 ;; Don’t compact font caches during GC
 inhibit-compacting-font-caches t
 ;; Increase single chunk bytes to read from subprocess (default 4096)
 read-process-output-max (if os/linux
                             (condition-case nil
                                 ;; Android may raise permission-denied error
                                 (with-temp-buffer
                                   (insert-file-contents
                                    "/proc/sys/fs/pipe-max-size")
                                   (string-to-number (buffer-string)))
                               ;; If an error occured, fallback to the default value
                               (error read-process-output-max))
                           (* 1024 1024))

 ;; ====== Aesthetics and UI ======
 ;; Do force frame size to be a multiple of char size
 frame-resize-pixelwise t
 ;; Stretch cursor to the glyph width
 x-stretch-cursor t
 ;; Show trailing whitespaces
 show-trailing-whitespace t
 ;; Resize window combinations proportionally
 window-combination-resize t
 ;; Enable time in the mode-line
 display-time-string-forms '((propertize (concat 24-hours ":" minutes)))
 ;; Relative line numbering
 ;;  display-line-numbers-type 'relative

 ;; Show unprettified symbol under cursor (when in `prettify-symbols-mode')
 prettify-symbols-unprettify-at-point t
 ;; Make tooltips last a bit longer (default 10s)
 tooltip-hide-delay 20
 ;; Use small frames to display tooltips instead of the default OS tooltips
 use-system-tooltips nil

 ;; ====== Undo ======
 ;; 10MB (default is 160kB)
 undo-limit 1000000
 ;; 50MB (default is 240kB)
 undo-strong-limit 5000000
 ;; 150MB (default is 24MB)
 undo-outer-limit 30000000

 ;; ====== Editing ======
 ;; Hitting TAB behavior
 tab-always-indent nil
 ;; Default behavior for `whitespace-cleanup'
 whitespace-action '(cleanup auto-cleanup)
 ;; End files with newline
 require-final-newline t

 ;; Enable Drag-and-Drop of regions
 mouse-drag-and-drop-region t
 ;; Enable Drag-and-Drop of regions from Emacs to external programs
 mouse-drag-and-drop-region-cross-program t

 ;; ====== Backups ======
 ;; Disable lockfiles
 create-lockfiles nil
 ;; Enable making backup files
 make-backup-files t
 ;; Number each backup file
 version-control t
 ;; Copy instead of renaming current file
 backup-by-copying t
 ;; Clean up after itself
 delete-old-versions t
 ;; Keep up to 5 old versions of each file
 kept-old-versions 5
 ;; Keep up to 5 new versions of each file
 kept-new-versions 5
 ;; Keep up to 5 versions when cleaning a directory
 dired-kept-versions 5

 ;; ====== Scrolling ======
 ;; Do not adjust window-vscroll to view tall lines. Fixes some lag issues see:
 ;; emacs.stackexchange.com/a/28746
 auto-window-vscroll nil
 ;; Fast scrolling
 fast-but-imprecise-scrolling t
 ;; Keep the point in the same position while scrolling
 scroll-preserve-screen-position t
 ;; Do not move cursor to the center when scrolling
 scroll-conservatively 101
 ;; Scroll at a margin of one line
 scroll-margin 1
 ;; Better scrolling on Emacs29+, specially on a touchpad
 pixel-scroll-precision-use-momentum t

 ;; ====== Recent files ======
 ;; Increase the maximum number of saved items
 recentf-max-saved-items 50
 ;; Ignore case when searching recentf files
 recentf-case-fold-search t
 ;; Exclude some files from being remembered by recentf
 recentf-exclude
 `(,(rx (* any)
     (or
      "elfeed-db"
      "eln-cache"
      "/cache/"
      ".maildir/"
      ".cache/"
      "node_modules")
     (* any)
     (? (or "html" "pdf" "tex" "epub")))
   ,(rx "/"
     (or "rsync" "ssh" "tmp" "yadm" "sudoedit" "sudo")
     (* any)))

 ;; ====== Timestamps ======
 ;; Do enable time-stamps
 time-stamp-active t
 ;; Check the first 12 buffer lines for Tire-stamp: <>
 time-stamp-line-limit 12
 ;; Timestamp format
 time-stamp-format "%04Y-%02m-%02d %02H:%02M:%02S"

 ;; ====== Auto-Saving, sessions ======
 ;; Enable auto-save (use `recover-file' or `recover-session' to recover)
 auto-save-default t
 ;; Include big deletions
 auto-save-include-big-deletions t
 ;; Set file naming transform
 auto-save-file-name-transforms (list
                                 ;; Prefix tramp autosaves with "tramp-"
                                 (list
                                  "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                  (concat auto-save-list-file-prefix "tramp-\\2") t)
                                 ;; Local autosaves
                                 (list ".*" auto-save-list-file-prefix t))
 ;; File name to use when saving desktop
 desktop-base-file-name "emacs-session.el"
 ;; File name to use as a lock
 desktop-base-lock-name (concat desktop-base-file-name ".lock")
 ;; Load only 5 buffers immediately, the remaining buffers will be loaded lazily
 desktop-restore-eager 5
 ;; Avoid writing contents unchanged between auto-saves
 desktop-file-checksum t
 desktop-save-mode t

 ;; ====== Compilation ======
 ;; Scroll compilation buffer
 compilation-scroll-output t ; 'first-error can be a good option
 ;; Always kill current compilation process before starting a new one
 compilation-always-kill t
 ;; Skip visited messages on compilation motion commands
 compilation-skip-visited t
 ;; Keep it readable
 compilation-window-height 12

 ;; ====== Misc ======
 ;; Set `webjump' sites to manily search engins
 webjump-sites
 '(("Emacs Wiki"    . [simple-query "www.emacswiki.org" "www.emacswiki.org/cgi-bin/wiki/" ""])
   ("DuckDuckGo"    . [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
   ("Qwant"         . [simple-query "www.qwant.com" "www.qwant.com/?q=" ""])
   ("Ecosia"        . [simple-query "www.ecosia.org" "www.ecosia.org/search?q=" ""])
   ("Brave"         . [simple-query "search.brave.com" "search.brave.com/search?q=" ""])
   ("Bing"          . [simple-query "www.bing.com" "www.bing.com/search?q=" ""])
   ("Yahoo"         . [simple-query "www.yahoo.com" "search.yahoo.com/search?p=" ""])
   ("Google"        . [simple-query "www.google.com" "www.google.com/search?q=" ""])
   ("Google Maps"   . [simple-query "www.google.com" "www.google.com/maps?q=" ""])
   ("Google Images" . [simple-query "www.google.com" "www.google.com/images?q=" ""])
   ("Google Groups" . [simple-query "groups.google.com" "groups.google.com/groups?q=" ""])
   ("StackOverflow" . [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" ""])
   ("GitHub Repo"   . [simple-query "github.com" "github.com/search?type=repositories&q=" ""])
   ("GitHub Code"   . [simple-query "github.com" "github.com/search?type=code&q=" ""])
   ("WolframAlpha"  . [simple-query "wolframalpha.com" "wolframalpha.com/input/?i=" ""])
   ("MDN"           . [simple-query "developer.mozilla.org" "developer.mozilla.org/search?q=" ""])
   ("Youtube"       . [simple-query "www.youtube.com" "www.youtube.com/results?search_query=" ""])
   ("Reddit"        . [simple-query "www.reddit.com" "www.reddit.com/search/?q=" ""])
   ("Wikipedia"     . [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])))

(setq-default
 ;; ====== Buffer-local variables ======
 ;; Display long lines
 truncate-lines nil
 ;; Default fill column width
 fill-column 120
 ;; Never mix, use only spaces
 indent-tabs-mode nil
 ;; Width for line numbers
 display-line-numbers-width 4
 ;; Small tab is enough!
 tab-width 2
 ;; Save buffer status
 desktop-save-buffer t
 ;; set line-height
 line-spacing 0.15)

;; ====== Misc hooks and advices ======
;; Advice `emacs-session-filename' to ensure creating "session.ID" files in
;; a sub-directory
(with-eval-after-load 'x-win
  (advice-add
   #'emacs-session-filename :filter-return
   (defun +emacs-session-filename--in-subdir-a (session-filename)
     "Put the SESSION-FILENAME in the \"x-win/\" sub-directory."
     (concat (+directory-ensure rivenemacs-local-dir "x-win/")
             (file-name-nondirectory session-filename)))))

;; Kill `term' buffer on exit (reproduce a similar behavior to `shell's
;; `shell-kill-buffer-on-exit').
(advice-add
 'term-sentinel :around
 (defun +term--kill-after-exit-a (orig-fn proc msg)
   (if (memq (process-status proc) '(signal exit))
       (let ((buffer (process-buffer proc)))
         (apply orig-fn (list proc msg))
         (kill-buffer buffer))
     (apply orig-fn (list proc msg)))))

;; Kill the minibuffer when switching by mouse to another window.
;; Adapted from: trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(add-hook
 'mouse-leave-buffer-hook
 (defun +minibuffer--kill-on-mouse-h ()
   "Kill the minibuffer when switching to window with mouse."
   (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
     (abort-recursive-edit))))

;; ====== Tweaks on file save ======
;; Update time stamp (if available) before saving a file.
(add-hook 'before-save-hook 'time-stamp)

(defcustom +whitespace-auto-cleanup-modes
  '(prog-mode conf-mode org-mode markdown-mode
    latex-mode tex-mode bibtex-mode)
  "Enable auto white space cleanup before saving for these derived modes."
  :group 'rivenemacs-edit
  :type '(repeat symbol))

;; Auto-remove trailing white spaces before saving for modes defined in
;; `+whitespace-auto-cleanup-modes'.
(add-hook
 'before-save-hook
 (defun +save--whitespace-cleanup-h ()
   (when (cl-some #'derived-mode-p +whitespace-auto-cleanup-modes)
     (whitespace-cleanup))))

;; Guess the major mode after saving a file in `fundamental-mode' (adapted
;; from Doom Emacs).
(add-hook
 'after-save-hook
 (defun +save--guess-file-mode-h ()
   "Guess major mode when saving a file in `fundamental-mode'.
Likely, something has changed since the buffer was opened. e.g. A shebang line
or file path may exist now."
   (when (eq major-mode 'fundamental-mode)
     (let ((buffer (or (buffer-base-buffer) (current-buffer))))
       (and (buffer-file-name buffer)
            (eq buffer (window-buffer (selected-window))) ;; Only visible buffers
            (set-auto-mode))))))

;; ====== Modes enabled locally, mainly for `prog-mode', `conf-mode' and `text-mode' ======
;; Show line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; Highlight the current line
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'conf-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;; Hide/show code blocks, a.k.a. code folding
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'conf-mode-hook #'hs-minor-mode)

;; Wrap long lines
(add-hook 'prog-mode-hook #'visual-line-mode)
(add-hook 'conf-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

;; When RivenEmacs is running in an asynchronous Org export context, there is no
;; need to enable these modes. So we load them only if we haven't been launched
;; through the `re-org-export-async-init' file.
;; All modes and tweaks are enabled after RivenEmacs is gets loaded
(+deferred-unless! (featurep 're-org-export-async-init)
  ;; Navigate windows using Shift+Direction
  (windmove-default-keybindings)

  ;; ====== Modes enabled globally ======
  ;; Show the battery status (if available) in the mode-line
  (+shutup!
   (let ((battery-str (battery)))
     (unless (or (equal "Battery status not available" battery-str)
                 (string-match-p "unknown" battery-str)
                 (string-match-p "N/A" battery-str))
       (display-battery-mode 1))))

  ;; Fallback the new `fido-vertical-mode' Emacs28+ builtin completion mode if
  ;; the `re-completion' (which contains `vertico-mode' configuration) core
  ;; module is not enabled.
  (unless (memq 're-completion rivenemacs-core-modules)
    (fido-vertical-mode 1))

  ;; Window layout undo/redo (`winner-undo' / `winner-redo')
  (winner-mode 1)

  ;; Scroll pixel by pixel, in Emacs29+ there is a more pricise mode way to scroll
  (if (>= emacs-major-version 29)
      (pixel-scroll-precision-mode 1)
    (pixel-scroll-mode 1))

  ;; Display time in mode-line
  (display-time-mode 1)

  ;; Replace selection after start typing
  (delete-selection-mode 1)

  ;; Enable `recentf-mode' to remember recent files
  (+shutup! (recentf-mode 1))

  ;; Show recursion depth in minibuffer (see `enable-recursive-minibuffers')
  (minibuffer-depth-indicate-mode 1)

  ;; Save place in files
  (save-place-mode 1)

  ;; Enable saving minibuffer history
  (savehist-mode 1)

  ;; Auto load files changed on disk
  (global-auto-revert-mode 1)

  ;; Show line number in mode-line
  (line-number-mode 1)

  ;; Show column numbers (a.k.a. cursor position) in the mode-line
  (column-number-mode 1)

  ;; Better handling for files with so long lines
  (global-so-long-mode 1)

  ;; Save Emacs state from one session to another
  (desktop-save-mode 1)

  ;; Global SubWord mode
  (global-subword-mode 1))


(provide 're-defaults)
