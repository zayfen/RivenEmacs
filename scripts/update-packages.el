;;; update-packages.el --- Batch package upgrader for RivenEmacs  -*- lexical-binding: t; -*-

;;; Commentary:

;; Upgrade both packages installed with package.el (ELPA archives) and
;; checkouts managed by package-vc (Git repositories).
;;
;; Run this through scripts/update-packages.sh:
;;
;;   scripts/update-packages.sh --dry-run   ; show what would change
;;   scripts/update-packages.sh             ; upgrade everything
;;
;; ELPA packages are upgraded through `package-upgrade', which is the same
;; operation as `M-x package-upgrade-all' without the confirmation prompt.
;;
;; package-vc checkouts are updated in place with Git, synchronously:
;;
;;   - a checkout on a branch is fast-forwarded to its upstream;
;;   - a checkout detached at a release commit (tagged or not) is advanced
;;     to the newest release commit (the last commit that changed the
;;     Version header);
;;   - a checkout detached at a remote branch tip (use-package's
;;     `:rev :newest' semantics, or a repository without release metadata)
;;     is advanced to the new tip;
;;   - a checkout detached at a pinned commit, or one with tracked local
;;     changes, is skipped and reported.
;;
;; After moving a checkout, `package-vc-rebuild' regenerates autoloads,
;; package metadata and byte code.  Native compilation is left to the next
;; Emacs start so the updater stays reasonably fast.
;;
;; The updater never edits init files or custom.el.  Restart Emacs after a
;; successful run so newly installed versions are loaded.

;;; Code:

(require 'package)
(require 'package-vc)
(require 'seq)
(require 'subr-x)

(defvar riven/update-packages-auto-run nil
  "When non-nil, run the updater immediately after loading.
`scripts/update-packages.sh' sets this before loading this file.")

(defconst riven/update-packages--usage
  "Usage: scripts/update-packages.sh [OPTIONS]

Upgrade ELPA packages and package-vc (Git) checkouts.

Options:
  --dry-run       Show what would be upgraded; make no changes.
  --elpa-only     Only upgrade package.el packages.
  --vc-only       Only upgrade package-vc checkouts.
  --no-refresh    Reuse the cached ELPA archive index (no download).
  --no-fetch      Do not fetch VC remotes; use local refs only.
  --allow-build   Allow :make/:shell-command build steps for VC packages
                  (needed for example by `reader'; disabled by default).
  --only PKG      Only consider PKG (repeatable).
  -h, --help      Show this help.

Environment:
  EMACS           Emacs executable to use (default: emacs).

Examples:
  scripts/update-packages.sh --dry-run
  scripts/update-packages.sh --elpa-only
  scripts/update-packages.sh --vc-only --only vertico
  scripts/update-packages.sh --allow-build
")

;;; Logging

(defun riven/update-packages--log (format-string &rest args)
  "Log a progress message using FORMAT-STRING and ARGS."
  (message "[update] %s" (apply #'format format-string args)))

(defun riven/update-packages--warn (format-string &rest args)
  "Log a warning using FORMAT-STRING and ARGS."
  (message "[update] WARNING: %s" (apply #'format format-string args)))

;;; Options

(defun riven/update-packages--parse-args (args)
  "Parse command-line ARGS into an option plist.
ARGS is a list of strings.  Signals `user-error' for invalid input."
  (let ((opts (list :dry-run nil :elpa t :vc t :refresh t :fetch t
                    :allow-build nil :only nil :help nil)))
    (while args
      (let ((arg (pop args)))
        (cond
         ((member arg '("-h" "--help"))
          (setq opts (plist-put opts :help t)))
         ((equal arg "--dry-run")
          (setq opts (plist-put opts :dry-run t)))
         ((equal arg "--elpa-only")
          (setq opts (plist-put opts :vc nil)))
         ((equal arg "--vc-only")
          (setq opts (plist-put opts :elpa nil)))
         ((equal arg "--no-refresh")
          (setq opts (plist-put opts :refresh nil)))
         ((equal arg "--no-fetch")
          (setq opts (plist-put opts :fetch nil)))
         ((equal arg "--allow-build")
          (setq opts (plist-put opts :allow-build t)))
         ((equal arg "--only")
          (unless args
            (user-error "`--only' requires a package name"))
          (let ((name (intern (pop args))))
            (setq opts (plist-put opts :only
                                  (append (plist-get opts :only)
                                          (list name))))))
         (t
          (user-error "Unknown argument: %s" arg)))))
    (when (and (not (plist-get opts :elpa))
               (not (plist-get opts :vc)))
      (user-error "Nothing to do: `--elpa-only' and `--vc-only' cancel out"))
    opts))

;;; Small helpers

(defun riven/update-packages--version (name source)
  "Return NAME's version string from SOURCE, or nil."
  (when-let* ((desc (package-get-descriptor name source)))
    (package-version-join (package-desc-version desc))))

(defun riven/update-packages--short (commit)
  "Return a short display form of COMMIT."
  (if (and (stringp commit) (>= (length commit) 8))
      (substring commit 0 8)
    (or commit "unknown")))

;;; ELPA packages

(defun riven/update-packages--refresh-elpa ()
  "Refresh ELPA archive contents.  Return non-nil on success."
  (riven/update-packages--log "refreshing ELPA archive contents")
  (condition-case err
      (progn
        (package-refresh-contents)
        t)
    (error
     (riven/update-packages--warn "ELPA refresh failed: %s"
                                  (error-message-string err))
     nil)))

(defun riven/update-packages--elpa-upgradable (only)
  "Return upgradeable ELPA package names, optionally filtered by ONLY.
VC packages are excluded by `package--upgradeable-packages'."
  (let ((names (package--upgradeable-packages)))
    (if only
        (seq-filter (lambda (name) (memq name only)) names)
      names)))

(defun riven/update-packages--upgrade-elpa (opts)
  "Upgrade ELPA packages according to OPTS.
Return a plist (:ok N :failed N :failures ALIST)."
  (let* ((dry (plist-get opts :dry-run))
         (upgradable (riven/update-packages--elpa-upgradable
                      (plist-get opts :only)))
         (ok 0)
         (failed 0)
         failures)
    (cond
     ((null upgradable)
      (riven/update-packages--log "ELPA: nothing to upgrade"))
     (t
      (riven/update-packages--log "ELPA: %d package(s) to upgrade%s"
                                  (length upgradable)
                                  (if dry " (dry run)" ""))
      (dolist (name upgradable)
        (let ((old (riven/update-packages--version name 'installed))
              (new (riven/update-packages--version name 'archive)))
          (riven/update-packages--log "  %s: %s -> %s" name old new)
          (cond
           (dry
            (setq ok (1+ ok)))
           (t
            (condition-case err
                (progn
                  (package-upgrade name)
                  (setq ok (1+ ok))
                  (riven/update-packages--log "  upgraded %s" name))
              (error
               (setq failed (1+ failed))
               (push (cons name (error-message-string err)) failures)
               (riven/update-packages--warn "  failed to upgrade %s: %s"
                                            name
                                            (error-message-string err))))))))))
    (list :ok ok :failed failed :failures (nreverse failures))))

;;; VC packages

(defun riven/update-packages--vc-packages (&optional only)
  "Return an alist of (NAME . DESC) for installed VC packages.
When ONLY is non-nil, keep only packages whose name is in ONLY."
  (let (result)
    (dolist (entry package-alist)
      (let ((name (car entry))
            (desc (car (cdr entry))))
        (when (and desc
                   (package-vc-p desc)
                   (or (null only) (memq name only)))
          (push (cons name desc) result))))
    (nreverse result)))

(defun riven/update-packages--git (dir &rest args)
  "Run Git with ARGS in DIR and return (STATUS . OUTPUT).
STATUS is an integer exit code or a signal description."
  (with-temp-buffer
    (let ((status (apply #'process-file
                         "git" nil t nil
                         "-C" dir args)))
      (cons status (string-trim (buffer-string))))))

(defun riven/update-packages--git-string (dir &rest args)
  "Run Git with ARGS in DIR and return stdout when it succeeds."
  (pcase-let ((`(,status . ,output)
               (apply #'riven/update-packages--git dir args)))
    (when (and (integerp status) (zerop status))
      output)))

(defun riven/update-packages--git-check (dir &rest args)
  "Run Git with ARGS in DIR, signaling an error on failure.
Return stdout on success."
  (pcase-let ((`(,status . ,output)
               (apply #'riven/update-packages--git dir args)))
    (unless (and (integerp status) (zerop status))
      (error "git %s failed in %s: %s"
             (mapconcat #'identity args " ")
             dir
             (if (string-empty-p output)
                 (format "exit %s" status)
               output)))
    output))

(defun riven/update-packages--checkout-dir (desc)
  "Return the Git checkout directory for package DESC."
  (or (ignore-errors (package-vc--checkout-dir desc))
      (package-desc-dir desc)))

(defun riven/update-packages--resolve (dir ref)
  "Resolve Git REF in DIR to a commit hash, or nil."
  (riven/update-packages--git-string
   dir "rev-parse" "--verify" "--quiet" (concat ref "^{commit}")))

(defun riven/update-packages--head (dir)
  "Return the current commit hash in DIR."
  (riven/update-packages--resolve dir "HEAD"))

(defun riven/update-packages--default-ref (dir)
  "Return the remote default branch ref for DIR, or nil.
Only local refs are consulted; `git fetch' first when it may be stale."
  (or (when-let* ((head (ignore-errors
                          (riven/update-packages--git-string
                           dir "symbolic-ref" "-q" "refs/remotes/origin/HEAD"))))
        (string-remove-prefix "refs/remotes/" head))
      (seq-find (lambda (ref)
                  (riven/update-packages--resolve dir ref))
                '("origin/main" "origin/master"))))

(defun riven/update-packages--current-branch (dir)
  "Return the current local branch name in DIR, or nil when detached."
  (let ((branch (riven/update-packages--git-string
                 dir "symbolic-ref" "-q" "--short" "HEAD")))
    (and branch (not (string-empty-p branch)) branch)))

(defun riven/update-packages--at-tag (dir)
  "Return the exact tag at HEAD in DIR, or nil."
  (riven/update-packages--git-string dir "describe" "--tags" "--exact-match" "HEAD"))

(defun riven/update-packages--ancestor-p (dir ancestor commit)
  "Return non-nil when ANCESTOR is an ancestor of COMMIT in DIR."
  (pcase-let ((`(,status . ,_output)
               (riven/update-packages--git dir "merge-base" "--is-ancestor"
                                           ancestor commit)))
    (and (integerp status) (zerop status))))

(defun riven/update-packages--upstream-ref (dir branch)
  "Return the configured upstream ref for BRANCH in DIR, or nil."
  (let ((upstream (riven/update-packages--git-string
                   dir "for-each-ref" "--format=%(upstream:short)"
                   (concat "refs/heads/" branch))))
    (and upstream (not (string-empty-p upstream)) upstream)))

(defun riven/update-packages--vc-rebuild (name desc allow-build)
  "Rebuild NAME's VC installation using DESC.
When ALLOW-BUILD is non-nil, permit :make/:shell-command build steps.
Native compilation is left for the next Emacs start."
  (riven/update-packages--log "  rebuilding %s" name)
  (let ((package-native-compile nil)
        (package-vc-allow-build-commands
         (if allow-build t package-vc-allow-build-commands)))
    (package-vc-rebuild desc)))

(defun riven/update-packages--vc-restore-checkout (dir commit)
  "Best-effort restore DIR to COMMIT (detached)."
  (ignore-errors
    (riven/update-packages--git-check dir "checkout" "-q" "--detach" commit)))

(defun riven/update-packages--vc-restore-branch (dir commit)
  "Best-effort reset DIR's branch to COMMIT."
  (ignore-errors
    (riven/update-packages--git-check dir "reset" "--hard" "-q" commit)))

(defun riven/update-packages--vc-report (name result)
  "Report the RESULT plist for package NAME."
  (let ((status (plist-get result :status))
        (old (plist-get result :old))
        (new (plist-get result :new))
        (message (plist-get result :message))
        (version (plist-get result :version)))
    (pcase status
      ('upgraded
       (riven/update-packages--log
        "  %s: %s -> %s%s"
        name (riven/update-packages--short old) (riven/update-packages--short new)
        (if version (format " (v%s, %s)" version (or message "updated")) "")))
      ('would-upgrade
       (riven/update-packages--log
        "  %s: %s -> %s (would update%s)"
        name (riven/update-packages--short old) (riven/update-packages--short new)
        (if message (format "; %s" message) "")))
      ('unchanged
       (riven/update-packages--log
        "  %s: up to date (%s)" name (riven/update-packages--short old)))
      ('skipped
       (riven/update-packages--warn "  %s: skipped (%s)" name message))
      ('failed
       (riven/update-packages--warn "  %s: FAILED (%s)" name message))
      (_
       (riven/update-packages--log "  %s: %s" name status)))))

(defun riven/update-packages--vc-update-branch (name dir desc branch original opts)
  "Update BRANCH checkout of NAME (DESC in DIR) from ORIGINAL.
OPTS is the parsed option plist.  Return a result plist."
  (let* ((target (or (riven/update-packages--upstream-ref dir branch)
                     (and (riven/update-packages--resolve dir
                                                          (concat "origin/" branch))
                          (concat "origin/" branch))))
         (dry (plist-get opts :dry-run)))
    (cond
     ((null target)
      (list :status 'skipped :message "branch has no upstream"))
     ((null (riven/update-packages--resolve dir target))
      (list :status 'skipped :message (format "cannot resolve %s" target)))
     ((equal (riven/update-packages--resolve dir target) original)
      (list :status 'unchanged :old original :new original :message target))
     (dry
      (list :status 'would-upgrade
            :old original
            :new (riven/update-packages--resolve dir target)
            :message (format "fast-forward to %s" target)))
     (t
      (riven/update-packages--log "  fast-forwarding %s to %s" name target)
      (condition-case err
          (progn
            (riven/update-packages--git-check dir "merge" "--ff-only" target)
            (riven/update-packages--vc-rebuild name desc
                                               (plist-get opts :allow-build))
            (list :status 'upgraded
                  :old original
                  :new (riven/update-packages--head dir)
                  :version (riven/update-packages--version name 'installed)
                  :message target))
        (error
         (riven/update-packages--vc-restore-branch dir original)
         (ignore-errors (riven/update-packages--vc-rebuild name desc nil))
         (list :status 'failed
               :old original
               :message (error-message-string err))))))))

(defun riven/update-packages--current-release (desc)
  "Return the release commit of the installed checkout for DESC, or nil.
The release commit is the last commit that changed the Version header."
  (let* ((main-file (ignore-errors (package-vc--main-file desc)))
         (release (ignore-errors (package-vc--release-rev desc)))
         (buffer (and main-file (get-file-buffer main-file))))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    release))

(defun riven/update-packages--vc-update-detached
    (name dir desc original prev-tip release-current opts)
  "Update detached checkout of NAME (DESC in DIR).
ORIGINAL is the current commit, PREV-TIP the remote tip before fetching and
RELEASE-CURRENT the release commit of the installed checkout (if any).
OPTS is the parsed option plist.  Return a result plist."
  (let* ((default-ref (riven/update-packages--default-ref dir))
         (remote-tip (and default-ref
                          (riven/update-packages--resolve dir default-ref)))
         (at-tag (riven/update-packages--at-tag dir))
         (release-mode (or (equal release-current original)
                           (and at-tag t)))
         (newest-mode (and (not release-mode)
                           (or (equal original prev-tip)
                               (and (null release-current)
                                    remote-tip
                                    (riven/update-packages--ancestor-p
                                     dir original remote-tip)))))
         (dry (plist-get opts :dry-run)))
    (cond
     ((null remote-tip)
      (list :status 'skipped :message "cannot resolve the remote default branch"))
     ((equal original remote-tip)
      (list :status 'unchanged :old original :new original :message default-ref))
     ((not (or release-mode newest-mode))
      (list :status 'skipped :message "detached at a pinned commit"))
     (dry
      (list :status 'would-upgrade
            :old original
            :new remote-tip
            :message (if release-mode
                         "would advance to the newest release commit"
                       "would advance to the new remote tip")))
     (release-mode
      ;; Move to the remote tip first so `package-vc--release-rev' can find
      ;; the newest release commit in the remote history, then check out
      ;; that release (or the tip when the package has no Version header).
      (condition-case err
          (progn
            (riven/update-packages--git-check dir "checkout" "-q" "--detach"
                                              remote-tip)
            (let* ((release (ignore-errors (package-vc--release-rev desc)))
                   (target (or release remote-tip))
                   (target-hash (riven/update-packages--resolve dir target)))
              (unless target-hash
                (error "Cannot resolve target revision %s" target))
              (cond
               ((equal target-hash original)
                (riven/update-packages--vc-restore-checkout dir original)
                (list :status 'unchanged :old original :new original
                      :message "already at the latest release"))
               ((riven/update-packages--ancestor-p dir target-hash original)
                ;; The installed commit is newer than the remote release
                ;; (for example a tag on a side branch).  Never downgrade.
                (riven/update-packages--vc-restore-checkout dir original)
                (list :status 'unchanged :old original :new original
                      :message "installed checkout is newer than the remote release"))
               (t
                (riven/update-packages--git-check dir "checkout" "-q" "--detach"
                                                  target-hash)
                (riven/update-packages--vc-rebuild name desc
                                                   (plist-get opts :allow-build))
                (list :status 'upgraded
                      :old original
                      :new (riven/update-packages--head dir)
                      :version (riven/update-packages--version name 'installed)
                      :message (if release "latest release" "latest commit"))))))
        (error
         (riven/update-packages--vc-restore-checkout dir original)
         (ignore-errors (riven/update-packages--vc-rebuild name desc nil))
         (list :status 'failed
               :old original
               :message (error-message-string err)))))
     (t
      ;; Newest mode: the checkout was installed at a branch tip and has no
      ;; release metadata, so follow the remote branch tip.
      (condition-case err
          (progn
            (riven/update-packages--log "  moving %s to %s" name default-ref)
            (riven/update-packages--git-check dir "checkout" "-q" "--detach"
                                              remote-tip)
            (riven/update-packages--vc-rebuild name desc
                                               (plist-get opts :allow-build))
            (list :status 'upgraded
                  :old original
                  :new (riven/update-packages--head dir)
                  :version (riven/update-packages--version name 'installed)
                  :message "remote tip"))
        (error
         (riven/update-packages--vc-restore-checkout dir original)
         (ignore-errors (riven/update-packages--vc-rebuild name desc nil))
         (list :status 'failed
               :old original
               :message (error-message-string err))))))))

(defun riven/update-packages--vc-upgrade-one (name desc opts)
  "Upgrade one VC package NAME with DESC according to OPTS.
Return a result plist."
  (let ((dir (riven/update-packages--checkout-dir desc)))
    (cond
     ((not (file-directory-p dir))
      (list :status 'failed :message "checkout directory is missing"))
     ((null (riven/update-packages--git-string dir "rev-parse" "--git-dir"))
      (list :status 'skipped :message "not a Git checkout"))
     (t
      (let ((local-changes
             (riven/update-packages--git-string
              dir "status" "--porcelain" "--untracked-files=no")))
        (cond
         ((and local-changes (not (string-empty-p local-changes)))
          (list :status 'skipped :message "tracked local changes; refusing to update"))
         (t
          (let* ((original (riven/update-packages--head dir))
                 (branch (riven/update-packages--current-branch dir))
                 (default-ref (riven/update-packages--default-ref dir))
                 (prev-tip (and default-ref
                                (riven/update-packages--resolve dir default-ref))))
            (when (plist-get opts :fetch)
              (riven/update-packages--log "  fetching %s" name)
              (riven/update-packages--git-check dir "fetch" "--tags" "--prune"
                                                "origin"))
            (if branch
                (riven/update-packages--vc-update-branch name dir desc branch
                                                          original opts)
              (let ((release-current
                     (riven/update-packages--current-release desc)))
                (riven/update-packages--vc-update-detached
                 name dir desc original prev-tip release-current opts)))))))))))

(defun riven/update-packages--upgrade-vc (opts)
  "Upgrade VC packages according to OPTS.
Return a plist (:ok N :failed N :skipped N :failures ALIST)."
  (let* ((dry (plist-get opts :dry-run))
         (packages (riven/update-packages--vc-packages (plist-get opts :only)))
         (ok 0)
         (failed 0)
         (skipped 0)
         failures)
    (cond
     ((null packages)
      (riven/update-packages--log "VC: no matching package"))
     (t
      (riven/update-packages--log "VC: %d package(s)%s"
                                  (length packages)
                                  (if dry " (dry run)" ""))
      (dolist (entry packages)
        (let* ((name (car entry))
               (result (condition-case err
                           (riven/update-packages--vc-upgrade-one
                            name (cdr entry) opts)
                         (error
                          (list :status 'failed
                                :message (error-message-string err))))))
          (riven/update-packages--vc-report name result)
          (pcase (plist-get result :status)
            ((or 'upgraded 'would-upgrade 'unchanged)
             (setq ok (1+ ok)))
            ('skipped
             (setq skipped (1+ skipped)))
            ('failed
             (setq failed (1+ failed))
             (push (cons name (plist-get result :message)) failures))
            (_
             (setq failed (1+ failed))
             (push (cons name (format "unexpected status %s"
                                      (plist-get result :status)))
                   failures)))))))
    (list :ok ok :failed failed :skipped skipped
          :failures (nreverse failures))))

;;; Entry point

(defun riven/update-packages--run (opts)
  "Run an update according to OPTS.  Return a process exit status."
  (let ((start (current-time))
        (package-native-compile nil))
    (riven/update-packages--log "Emacs %s; package-user-dir %s%s"
                                emacs-version package-user-dir
                                (if (plist-get opts :dry-run)
                                    " [dry run]"
                                  ""))
    (let (refresh-failed
          elpa-result
          vc-result)
      (when (plist-get opts :elpa)
        (when (plist-get opts :refresh)
          (unless (riven/update-packages--refresh-elpa)
            (setq refresh-failed t)))
        (setq elpa-result (riven/update-packages--upgrade-elpa opts)))
      (when (plist-get opts :vc)
        (setq vc-result (riven/update-packages--upgrade-vc opts)))
      (riven/update-packages--log "---- summary ----")
      (when elpa-result
        (riven/update-packages--log "ELPA: %d ok, %d failed"
                                    (plist-get elpa-result :ok)
                                    (plist-get elpa-result :failed)))
      (when vc-result
        (riven/update-packages--log "VC: %d ok, %d failed, %d skipped"
                                    (plist-get vc-result :ok)
                                    (plist-get vc-result :failed)
                                    (plist-get vc-result :skipped)))
      (when refresh-failed
        (riven/update-packages--warn
         "ELPA archive refresh failed; results may be based on stale data"))
      (riven/update-packages--log "finished in %.1fs; restart Emacs to load new versions"
                                  (float-time (time-since start)))
      (if (or refresh-failed
              (> (or (plist-get elpa-result :failed) 0) 0)
              (> (or (plist-get vc-result :failed) 0) 0))
          1
        0))))

(defun riven/update-packages-main ()
  "Run the package updater using arguments from the environment.
Return never (calls `kill-emacs')."
  (let* ((raw (or (getenv "RIVENEMACS_UPDATE_ARGS") ""))
         (args (split-string raw "\n" t)))
    (condition-case err
        (let ((opts (riven/update-packages--parse-args args)))
          (if (plist-get opts :help)
              (progn
                (princ riven/update-packages--usage)
                (kill-emacs 0))
            (kill-emacs (riven/update-packages--run opts))))
      (user-error
       (message "error: %s" (error-message-string err))
       (message "Run scripts/update-packages.sh --help for usage.")
       (kill-emacs 2))
      (error
       (message "error: %s" (error-message-string err))
       (kill-emacs 2)))))

(when riven/update-packages-auto-run
  (riven/update-packages-main))

(provide 'update-packages)
;;; update-packages.el ends here
