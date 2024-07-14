;; init.el --- RivenEmacs core initialization file -*- lexical-binding: t; -*-



;; Check if Emacs version is supported. You can define the
;; $RIVENEMACS_IGNORE_VERSION_CHECK environment variable to ignore this check.
;; This can be useful if you are stuck with an old Emacs version and you've
;; incrementally implemented the new Emacs routines RivenEmacs needs in your
;; "init-tweaks.el".
(let ((min-ver "28.0"))
  (when (and (version< emacs-version min-ver) (not (getenv "RIVENEMACS_IGNORE_VERSION_CHECK")))
    (error "Emacs v%s is not supported, RivenEmacs requires v%s or higher" emacs-version min-ver)))

;; PERF: Setting `file-name-handler-alist' to nil should boost startup time.
;; reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start
;; Store the current value so we can reset it after Emacs startup.
(put 'file-name-handler-alist 'original-value (default-toplevel-value 'file-name-handler-alist))
;; Make sure the new value survives any current let-binding.
(set-default-toplevel-value 'file-name-handler-alist nil)
;; After Emacs startup, we restore `file-name-handler-alist' while conserving
;; the potential edits made during startup.
(add-hook
 'emacs-startup-hook
 (defun +rivenemacs--restore-file-name-handler-alist-h ()
   (setq file-name-handler-alist
         (delete-dups
          (append file-name-handler-alist
                  (get 'file-name-handler-alist 'original-value)))))
 101)

;; HACK: At this point, RivenEmacs variables defined in `re-vars' should be
;; already loaded (in "early-init.el"). However, we double-check here and load
;; them if necessary in case Emacs has been loaded directly from "init.el"
;; without passing by "early-init.el". This can happen when we are running in a
;; `re-org-export-async-init' context, or if we use some bootstrapping mechanism
;; like Chemacs2.
(unless (featurep 're-vars)
  (load (expand-file-name "core/re-vars.el" (file-name-directory (file-truename load-file-name))) nil t))

(defun +load (&rest filename-parts)
  "Load a file, the FILENAME-PARTS are concatenated to form the file name."
  (let ((filename (mapconcat #'identity filename-parts nil)))
    (if (file-exists-p filename)
        (load filename nil (not rivenemacs-verbose))
      (user-error "[RivenEmacs:Error] Cannot load \"%s\", the file doesn't exists." filename))))

;; HACK: Most core and third-party packages depends on the
;; `user-emacs-directory' variable to store some cache information and generated
;; configuration files. However, this will mess with RivenEmacs' directory (which
;; defaults to `user-emacs-directory'). To keep the "~/.emacs.d/" directory
;; clean, we overwrite the `user-emacs-directory' with `rivenemacs-local-dir' so
;; all generated files gets stored in "~/.emacs.d/local/".
;; BUG: It is important to set this here and not in `re-vars' nor in
;; "early-init.el", otherwise, it won't work with Chemacs2-based installations.
(setq user-emacs-directory rivenemacs-local-dir)

;; HACK: Load Emacs 29 back ports for earlier Emacs versions. Note that I do
;; only back port a very small number of the functions/variables that I use at
;; early stage from Emacs29+ to be compatible with Emacs 28.2. For any Emacs
;; version less than 29, RivenEmacs will enable the `re-compat' module and load it
;; just after `re-bootstrap'. This module loads the `compat' package which
;; provide several forward compatibility functions, it is loaded at an early
;; stage to provide its functionality to the rest of the modules so we can use
;; some new features when configuring them.
(when (< emacs-major-version 29)
  (+load rivenemacs-modules-dir "re-backports-29.el"))

(setq
 ;; Enable debugging on error when Emacs is launched with the "--debug-init"
 ;; option or when the environment variable "$RIVENEMACS_DEBUG" is defined (see
 ;; `re-vars').
 debug-on-error rivenemacs-debug
 ;; Decrese the warning type to `:error', unless we are running in verbose mode
 warning-minimum-level (if rivenemacs-verbose :warning :error)
 warning-minimum-log-level warning-minimum-level
 ;; Make byte compilation less noisy
 byte-compile-warnings rivenemacs-verbose
 byte-compile-verbose rivenemacs-verbose)

;; Native compilation settings
(when (featurep 'native-compile)
  (setq
   ;; Silence compiler warnings as they can be pretty disruptive, unless we are
   ;; running in `rivenemacs-verbose' mode.
   native-comp-async-report-warnings-errors (when rivenemacs-verbose 'silent)
   native-comp-verbose (if rivenemacs-verbose 1 0) ; do not be too verbose
   native-comp-debug (if rivenemacs-debug 1 0)
   ;; Make native compilation happens asynchronously.
   native-comp-jit-compilation t)

  ;; Set the right directory to store the native compilation cache to avoid
  ;; messing with "~/.emacs.d/".
  (startup-redirect-eln-cache (concat rivenemacs-cache-dir "eln/")))

;; Add some of RivenEmacs' directories to `load-path'.
(setq load-path (append (list rivenemacs-core-dir rivenemacs-elisp-dir rivenemacs-extras-dir) load-path))

(defun rivenemacs-generate-loaddefs ()
  "Generate RivenEmacs' loaddefs file."
  (interactive)
  (when (file-exists-p rivenemacs-loaddefs-file)
    (delete-file rivenemacs-loaddefs-file))

  (loaddefs-generate
   (list rivenemacs-core-dir rivenemacs-elisp-dir rivenemacs-extras-dir)
   rivenemacs-loaddefs-file))

;; Some of RivenEmacs commands and libraries are defined to be auto-loaded. In
;; particular, these in the `rivenemacs-core-dir', `rivenemacs-elisp-dir', and
;; `rivenemacs-extras-dir' directories. The generated loaddefs file will be stored
;; in `rivenemacs-loaddefs-file'. We first regenerate the loaddefs file if it
;; doesn't exist.
(unless (file-exists-p rivenemacs-loaddefs-file)
  (rivenemacs-generate-loaddefs))

;; Then we load the loaddefs file
(+load rivenemacs-loaddefs-file)

;; Load user init tweaks from "$RIVENEMACSDIR/init-tweaks.el" when available
(let ((user-init-tweaks (concat rivenemacs-config-dir "init-tweaks.el")))
  (when (file-exists-p user-init-tweaks)
    (+load user-init-tweaks)))

;; HACK: When Emacs is launched from the terminal (in GNU/Linux), it inherits
;; the terminal's environment variables, which can be useful specially for
;; running commands under a custom "$PATH" directory. But when Emacs is launched
;; from the desktop manager (KDE, Gnome, etc.), it can omit the terminal's
;; environment variables. The way I solve this is by launching Emacs from
;; terminal, which gives Emacs the full environment variables of the invoking
;; terminal. Then I call the `+env-save' command, which reads the environment
;; variables defined in `+env-save-vars' and stores them in
;; "~/.emacs.d/local/system-env.el". This file is then loaded in the future
;; Emacs sessions (launched either from terminal or from GUI) by calling the
;; `+env-load' command.
(+env-load) ; Load environment variables when available.

;; NOTE: This is RivenEmacs' synchronization point. To get a fast Emacs startup,
;; RivenEmacs tries to defer loading most of its packages until this hook is
;; executed. This is managed by the `rivenemacs-loaded' and `rivenemacs-lazy'
;; pseudo-modules. After loading Emacs, the `emacs-startup-hook' gets executed,
;; we use this hook to profile the startup time, load the fonts and the theme,
;; and setup the *scratch* buffer content. Lastly we require the
;; `rivenemacs-loaded' synchronization module, which runs internally the
;; `rivenemacs-after-startup-hook' hooks and provide `rivenemacs-loaded' so the
;; packages loaded with `:after rivenemacs-loaded' can be loaded. The
;; `rivenemacs-loaded' will require `rivenemacs-lazy' when Emacs goes idle, this
;; pseudo-module provides `rivenemacs-lazy' so the packages loaded with `:after
;; rivenemacs-lazy' can be loaded then it incrementally run the hooks in
;; `rivenemacs-lazy-hook' when Emacs goes idle.
(defun +rivenemacs--loaded-h ()
  (+log! "=============== Loaded Emacs ===============")
  (+info! "Loaded Emacs in %s." (emacs-init-time))

  ;; When running in an async Org export context, there is no need to set
  ;; the fonts, load the theme or play with the scratch buffer.
  (unless (featurep 're-org-export-async-init)

    ;; Initially RivenEmacs loads the `doom-one-light' theme, and when
    ;; `rivenemacs-theme' is set in user configuration, it is loaded here.
    (+load-theme)

    (+log! "Filling scratch buffer content.")
    (+fill-scratch-buffer)

    ;; In `re-defaults', the `initial-major-mode' is set to `fundamental-mode'
    ;; to enhance startup time. However, I like to use the scratch buffer to
    ;; evaluate Elisp code, so we switch to Elisp mode in the scratch buffer
    ;; when Emacs is idle for 10 seconds.
    (+eval-when-idle-for! 10.0
      (setq initial-major-mode 'emacs-lisp-mode)
      (with-current-buffer (get-scratch-buffer-create)
        (emacs-lisp-mode))))

  ;; Require the virtual package to triggre loading packages depending on it
  (require 'rivenemacs-loaded))

;; Add it to the very begining of `emacs-startup-hook'
(add-hook 'emacs-startup-hook #'+rivenemacs--loaded-h -101)

;; ========= Load RivenEmacs packages and user customization =========
;; When running in an async Org export context, the used modules are set in
;; modules/extras/re-org-export-async-init.el, so we must not overrite them with
;; the user's enabled modules.
(if (featurep 're-org-export-async-init)
    (progn (message "Loading \"init.el\" in an org-export-async context.")
           (setq rivenemacs-not-lazy t)
           (require 'rivenemacs-loaded))
  ;; Load the default list of enabled modules (`rivenemacs-modules' and `rivenemacs-core-modules')
  (+load rivenemacs-core-dir "re-modules.el")

  ;; The modules.el file can override rivenemacs-modules and rivenemacs-core-modules
  (let ((user-conf-modules (concat rivenemacs-config-dir "modules.el")))
    (when (file-exists-p user-conf-modules)
      (+load user-conf-modules))))

;; NOTE: Ensure the `re-gc' module is in the core modules list. This module
;; enables the `gcmh-mode' package (a.k.a. the Garbage Collector Magic Hack).
;; This GCMH minimizes GC interference with the activity by using a high GC
;; threshold during normal use, then when Emacs is idling, GC is triggered and a
;; low threshold is set. In RivenEmacs, we set the threshold (`gc-cons-threshold'
;; variable) to an unlimited size in "early-init.el", this helps improving the
;; startup time, but needs to be set down to a more reasonable value after Emacs
;; gets loaded. The use of `gcmh-mode' ensures reverting this value so we don't
;; need to do it manually.
;; NOTE: Ensure the `re-defaults', `re-splash', `re-bootstrap' and `re-compat'
;; modules are in the right order. The `re-compat' should be loaded just after
;; `re-bootstrap' once `straight' and `use-package' are set up. This enables us
;; to use some of the new Emacs 29 functions even on earlier Emacs versions,
;; this can be useful when configuring the module's packages and adding new
;; functionality.
(setq rivenemacs-core-modules
      (delete-dups
       (append
        '(re-defaults)
        (when (memq 're-splash rivenemacs-core-modules) '(re-splash))
        '(re-bootstrap)
        (when (< emacs-major-version 29) '(re-compat))
        '(re-builtin re-gc)
        rivenemacs-core-modules)))

;; Load RivenEmacs modules
(dolist (module-file (append
                      (mapcar (apply-partially #'format "%s%s.el" rivenemacs-core-dir) rivenemacs-core-modules)
                      (mapcar (apply-partially #'format "%s%s.el" rivenemacs-modules-dir) rivenemacs-modules)))
  (+load module-file))

;; Write user custom variables to separate file instead of "init.el"
(setq custom-file (concat rivenemacs-config-dir "custom-vars.el"))

;; Load the custom variables file if it exists
(when (file-exists-p custom-file)
  (+load custom-file))

;; Load user configuration from "$RIVENEMACSDIR/config.el" when available
(let ((user-config (concat rivenemacs-config-dir "config.el")))
  (when (file-exists-p user-config)
    (+load user-config)))

(+lazy-when! (featurep 'native-compile)
  (+info! "Trying to clean outdated native compile cache")
  ;; Delete outdated natively compiled files when Emacs become idle
  (+shutup! (native-compile-prune-cache)))

(+log! "Loaded init.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:inherit (fixed-pitch default) :background "black" :foreground "#989898")))))

(set-face-attribute 'fringe nil :background "#000000")
