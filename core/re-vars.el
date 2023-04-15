;; re-vars.el --- RivenEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; RivenEmacs directories

(defgroup rivenemacs nil
  "RivenEmacs specific functionalities.")

(defgroup rivenemacs-core nil
  "RivenEmacs core tweaks."
  :group 'rivenemacs)

(defgroup rivenemacs-ui nil
  "RivenEmacs UI tweaks."
  :group 'rivenemacs)

(defgroup rivenemacs-edit nil
  "RivenEmacs editor tweaks."
  :group 'rivenemacs)

(defgroup rivenemacs-prog nil
  "RivenEmacs programming stuff."
  :group 'rivenemacs)

(defgroup rivenemacs-keybinding nil
  "RivenEmacs keybinding."
  :group 'rivenemacs)

(defgroup rivenemacs-utils nil
  "RivenEmacs utility functions."
  :group 'rivenemacs)

(defconst rivenemacs-config-dir
  (file-name-as-directory
   (or (getenv "MINEMACS_DIR")
       (getenv "MINEMACSDIR")
       "~/.rivenemacs.d/"))
  "RivenEmacs user customization directory.")

(defconst rivenemacs-debug
  (and (or (getenv "MINEMACS_DEBUG") init-file-debug) t)
  "RivenEmacs is started in debug mode.")

(defconst rivenemacs-verbose
  (and (or (getenv "MINEMACS_VERBOSE") rivenemacs-debug) t)
  "RivenEmacs is started in verbose mode.")

(defconst rivenemacs-not-lazy
  (or (daemonp) (and (getenv "MINEMACS_NOT_LAZY") t))
  "Load lazy packages (rivenemacs-lazy-hook) immediately.")

(defcustom rivenemacs-msg-level
  (let ((level (string-to-number (or (getenv "MINEMACS_MSG_LEVEL") ""))))
    (cond (rivenemacs-verbose 4)
          ((> level 0) level)
          (t 1)))
  "Level of printed messages.
1 - `+error!'
2 - `+info!'
3 - `+log!'
4 - `+debug!'"
  :group 'rivenemacs-core
  :type '(choice
          (const :tag "Error" 1)
          (const :tag "Info" 2)
          (const :tag "Log" 3)
          (const :tag "Debug" 4)))

;; Derive the root directory from this file path
(defconst rivenemacs-root-dir
  (abbreviate-file-name
   (file-name-directory
    (directory-file-name
     (file-name-directory (file-truename load-file-name))))))
(defconst rivenemacs-core-dir (concat rivenemacs-root-dir "core/"))
(defconst rivenemacs-assets-dir (concat rivenemacs-root-dir "assets/"))
(defconst rivenemacs-elisp-dir (concat rivenemacs-root-dir "elisp/"))
(defconst rivenemacs-modules-dir (concat rivenemacs-root-dir "modules/"))
(defconst rivenemacs-extras-dir (concat rivenemacs-modules-dir "extras/"))
(defconst rivenemacs-local-dir (concat rivenemacs-root-dir "local/"))
(defconst rivenemacs-cache-dir (concat rivenemacs-local-dir "cache/"))
(defconst rivenemacs-loaddefs-file (concat rivenemacs-core-dir "re-loaddefs.el"))

(defconst os/linux (and (memq system-type '(gnu gnu/linux)) t))
(defconst os/bsd (and (memq system-type '(darwin berkeley-unix gnu/kfreebsd)) t))
(defconst os/win (and (memq system-type '(cygwin windows-nt ms-dos)) t))
(defconst os/mac (eq system-type 'darwin))

;; Should return x86_64, aarch64, armhf, ...
(defconst sys/arch (intern (car (split-string system-configuration "-"))))

(defconst emacs/features
  (mapcar #'intern
          (mapcar (apply-partially #'string-replace "_" "-")
                  (mapcar #'downcase (split-string system-configuration-features))))
  "List of symbols representing Emacs' enabled features.
Compiled from the `system-configuration-features'.")

(defcustom rivenemacs-fonts nil
  "Fonts to use within RivenEmacs."
  :group 'rivenemacs-ui
  :type '(plist
          (:font-family string)
          (:font-size natnum)
          (:unicode-font-family string)
          (:variable-pitch-font-family string)
          (:variable-pitch-font-size natnum)))

(defcustom rivenemacs-leader-key "C-c"
  "RivenEmacs leader key."
  :group 'rivenemacs-keybinding
  :type 'string)

(defcustom rivenemacs-localleader-key "C-c l"
  "RivenEmacs local leader (a.k.a. mode specific) key sequence."
  :group 'rivenemacs-keybinding
  :type 'string)

(defcustom rivenemacs-executor-key "C-x"
  "RivenEmacs executor key."
  :group 'rivenemacs-keybinding
  :type 'string)

(defcustom rivenemacs-theme 'modus-vivendi
  "The theme of RivenEmacs."
  :group 'rivenemacs-ui
  :type 'symbol)

(defcustom rivenemacs-after-set-fonts-hook nil
  "Runs after setting RivenEmacs fonts, runs at the end of `+set-fonts'."
  :group 'rivenemacs-ui
  :type 'hook)

(defcustom rivenemacs-after-load-theme-hook nil
  "Runs after loading RivenEmacs theme, runs at the end of `+load-theme'."
  :group 'rivenemacs-ui
  :type 'hook)

(defcustom rivenemacs-after-startup-hook nil
  "This hook will be run after loading Emacs.

RivenEmacs hooks will be run in this order:
1. `rivenemacs-after-startup-hook'
2. `rivenemacs-lazy-hook'"
  :group 'rivenemacs-core
  :type 'hook)

(defcustom rivenemacs-lazy-hook nil
  "This hook will be run after loading Emacs, with laziness.

RivenEmacs hooks will be run in this order:
1. `rivenemacs-after-startup-hook'
2. `rivenemacs-lazy-hook'"
  :group 'rivenemacs-core
  :type 'hook)

;; Setup default fonts (depending on the OS)
(let ((mono-font (cond (os/linux "Ligalex Mono")
                       (os/win "Lucida Console")
                       (os/mac "monospace")))
      (varp-font (cond (os/linux "IBM Plex Mono")
                       (os/win "Tahoma")
                       (os/mac "monospace")))
      (unicode-font (cond (os/linux "Noto Sans Mono CJK SC")
                          (os/win "sans")
                          (os/mac "sans"))))
  (defconst rivenemacs-default-fonts
    `(:font-family ,mono-font
      :font-size 12
      :unicode-font-family ,unicode-font
      :variable-pitch-font-family ,varp-font
      :variable-pitch-font-size 12)
    "Default fonts of RivenEmacs."))

(defcustom +env-save-vars
  '("PATH" "MANPATH" "CMAKE_PREFIX_PATH" "PKG_CONFIG_PATH" "LSP_USE_PLISTS")
  "List of the environment variables to saved by `+env-save'.
You need to run Emacs from terminal to get the environment variables.
RivenEmacs then save them when calling `+env-save' to be used in GUI sessions as well."
  :group 'rivenemacs-core
  :type '(repeat string))



;; Load env file steal from doomemacs
(defun load-envvars-file (file &optional noerror)
  "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
unreadable. Returns the names of envvars that were changed."
  (if (null (file-exists-p file))
      (unless noerror
        (signal 'file-error (list "No envvar file exists" file)))
    (with-temp-buffer
      (insert-file-contents file)
      (when-let (env (read (current-buffer)))
        (setq-default
         process-environment
         (append env (default-value 'process-environment))
         exec-path
         (append (split-string (getenv "PATH") path-separator t)
                 (list exec-directory))
         shell-file-name
         (or (getenv "SHELL")
             (default-value 'shell-file-name)))
        env))))


(provide 're-vars)
