;;; re-keybindings.el --- Default keybindings -*- lexical-binding: t; -*-




(use-package which-key
  :straight t
  :hook (rivenemacs-after-startup . which-key-mode)
  :custom
  (which-key-idle-delay 1.0)
  (which-key-idle-secondary-delay nil)
  (which-key-ellipsis "..")
  (which-key-prefix-prefix "+")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-min-display-lines 3)
  (which-key-max-display-columns nil)
  ;; Allow a key binding to be modified by multiple rules in
  ;; `which-key-replacement-alist'
  (which-key-allow-multiple-replacements t)
  :config
  (setq
   which-key-replacement-alist
   (append
    which-key-replacement-alist
    (list
     '(("\\`g z" . "\\`evil-\\(?:mc\\|multiedit\\)-\\(.*\\)")    . (nil . "⌶·\\1"))
     '(("\\`g c" . "\\`evilnc-\\(.*\\)")                         . (nil . "#·\\1"))
     '(("\\`g" . "\\`[Ii]nfo[-:]?\\(?:a-\\)?\\(.*\\)")           . (nil . "ɩ·\\1"))
     '(("\\`SPC TAB" . "\\`tabspaces-\\(.*\\)")                  . (nil . "⭾·\\1"))
     '(("\\`SPC p" . "\\`\\+?\\(?:consult-\\)?project-\\(.*\\)") . (nil . "🅟·\\1"))
     '(("" . "\\`evil[-:]?\\(?:a-\\)?\\(.*\\)")                  . (nil . "ɛ·\\1")))))
  ;; Setup `which-key' integration with the minibuffer
  (which-key-setup-minibuffer))

(use-package general
  :straight t
  ;; PERF: Loading `general' early make Emacs very slow on startup.
  :demand t
  :config
  ;; Advise `define-key' to automatically unbind keys when necessary.
  (general-auto-unbind-keys)
  (defalias 'define-key! #'general-def)
  (defalias 'undefine-key! #'general-unbind)

  ;; Set up some basic equivalents (like `general-nmap') with short named
  ;; aliases (like `nmap') for VIM mapping functions.
  (general-evil-setup t)

  ;; Global leader
  (general-create-definer +rivenemacs--internal-map!
    ;; The order of states matters, the last is prioritized
    :keymaps 'override
    :prefix rivenemacs-leader-key)

  ;; Local leader
  (general-create-definer +rivenemacs--internal-map-local!
    :keymaps 'override
    :prefix rivenemacs-localleader-key)

  (general-create-definer +rivenemacs--executor-map!
    :keymaps 'override
    :prefix rivenemacs-executor-key)

  (+rivenemacs--executor-map!
    ">"   '(switch-to-next-buffer :wk "Next buffer")
    "<"   '(switch-to-prev-buffer :wk "Previous buffer")
    ";"   '(pp-eval-expression :wk "Eval expression")
    "tF" '(toggle-frame-fullscreen :wk "Toggle Fullscreen"))

  ;; Define the built-in global keybindings
  (+rivenemacs--internal-map!
    ;; ====== Top level functions ======
    "X"   #'org-capture

    ;; ====== Quit/Session ======
    "q"   '(nil :wk "quit/session")
    "qq"  #'save-buffers-kill-terminal
    "qQ"  #'kill-emacs
    "qS"  #'server-start
    "qR"  #'recover-session
    "qd"  #'desktop-read
    "qD"  #'desktop-lazy-complete
    "qs"  #'desktop-save

    ;; ====== Files ======
    "f"   '(nil :wk "file")
    "fm"  #'+move-this-file
    "fS"  '(write-file :wk "Save as ...")
    "f DEL"  #'+delete-this-file
    "fr" '(recentf-cleanup :wk "Cleanup Recent Files")
    "ft"  #'recover-this-file
    "fy"  #'+yank-this-file-name
    "fE"  `(,(+cmdfy! (dired (or rivenemacs-config-dir rivenemacs-root-dir)))
            :wk "User config directory")

    ;; ====== Buffers ======
    "b"   '(nil :wk "buffer")
    "bi"  #'ibuffer
    "bs"  #'save-some-buffers
    "bM"  #'view-echo-area-messages
    "bK"  #'+kill-some-buffers
    "bk"  `(,(+cmdfy! (kill-buffer (current-buffer)))
            :wk "Kill this buffer")
    "br"  '(rename-buffer :wk "Rename")


    ;; Files / Local variables
    "bv"  '(nil :wk "locals")
    "bvv" '(add-file-local-variable :wk "Add")
    "bvV" '(delete-file-local-variable :wk "Delete")
    "bvp" '(add-file-local-variable-prop-line :wk "Add in prop line")
    "bvP" '(delete-file-local-variable-prop-line :wk "Delete from prop line")
    "bvd" '(add-dir-local-variable :wk "Add to dir-locals")
    "bvD" '(delete-dir-local-variable :wk "Delete from dir-locals")
    "bvr"  '(nil :wk "reload dir-locals for...")
    "bvrr" '(+dir-locals-reload-for-this-buffer :wk "This buffer")
    "bvrd" '(+dir-locals-reload-for-all-buffers-in-this-directory :wk "All buffers in this directory")

    ;; ====== Insert ======
    "i"   '(nil :wk "insert")
    "iu"  '(insert-char :wk "Unicode char")
    "ie"  `(,(when (>= emacs-major-version 29) #'emoji-search) :wk "Emoji")

    ;; ====== Window ======
    "w"   '(nil :wk "window")
    "wd"  #'delete-window
    "wD"  #'delete-window-on
    "wm"  #'maximize-window
    "wu"  #'winner-undo
    "wU"  #'winner-redo

    ;; ====== Applications (Open) ======
    "o"   '(nil   :wk "app/open")
    "o-"  '(dired :wk "Dired") ;; Will be overwritten if dirvish is used
    "oe"  #'eshell
    "of" #'elfeed

    ;; ====== Search ======
    "s"   '(nil :wk "search")
    "sw"  '+webjump

    ;; ====== VC ======
    "g"   '(nil :wk "git/vc")

    ;; ====== Workspaces ======

    ;; ====== Toggle ======
    "t"   '(nil :wk "toggle")
    "td"  '(toggle-debug-on-error :wk "Debug on error")
    "tr"  #'read-only-mode
    "tl"  #'follow-mode
    "tV"  '(netextender-toggle :wk "NetExtender")
    "tv"  #'visible-mode

    ;; ====== Code ======
    "c"   '(nil :wk "code")

    ;; ====== Workspaces ======
    "r"   '(nil :wk "workspace") ;; TODO: use tab-bar-mode and tab-line-mode

    ;; Bookmarks
    "m"  '(nil :wk "bookmark")
    "mm"  #'bookmark-set
    "md"  #'bookmark-delete
    "ml"  #'consult-bookmark

    ;; ====== Notes ======
    "n"   '(nil :wk "notes")

    ;; ====== Help ======
    "h"   '(nil :wk "help")
    "hd"  #'devdocs-lookup
    "hi"  #'info
    "hs"  #'+screenshot-svg
    "hm" #'consult-man

    ;; ====== Project ======
    "p"   '(nil :wk "project")
    "pe"  #'projectile-recentf)

  ;; HACK: This is a synchronization feature, providing `re-general-ready' tells
  ;; the `+map!', `+map-local!', ... macros that `general' is ready and the
  ;; definers `+rivenemacs--internal-map!', `+rivenemacs--internal-map-local!', ...
  ;; are available (See the `+map!' macro definition in "elisp/+rivenemacs.el").
  (provide 're-general-ready))

(use-package hydra
  :straight t)

;; (global-set-key (kbd "M-g f") 'forward-whitespace)
(global-set-key (kbd "M-i") 'hs-toggle-hiding)

(provide 're-emacs-keybindings)

;;; keybindings.el ends here
