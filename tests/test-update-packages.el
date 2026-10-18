;;; test-update-packages.el --- Tests for scripts/update-packages.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Argument parsing and option handling for the package update tool.  The
;; updater itself is exercised by hand against the real package directory;
;; these tests keep the CLI contract stable.

;;; Code:

(require 'ert)

(declare-function riven/update-packages--parse-args "update-packages" (args))

(defconst riven/test-update-packages--script
  (expand-file-name "scripts/update-packages.el"
                    (or (locate-dominating-file
                         (file-name-directory (or load-file-name
                                                  buffer-file-name
                                                  default-directory))
                         "scripts/update-packages.el")
                        default-directory))
  "Location of the update tool loaded by these tests.")

(load riven/test-update-packages--script t t)

(ert-deftest riven/update-packages-default-options ()
  "Defaults upgrade both ELPA and VC packages and refresh/fetch."
  (let ((opts (riven/update-packages--parse-args nil)))
    (should (plist-get opts :elpa))
    (should (plist-get opts :vc))
    (should (plist-get opts :refresh))
    (should (plist-get opts :fetch))
    (should-not (plist-get opts :dry-run))
    (should-not (plist-get opts :allow-build))
    (should-not (plist-get opts :only))))

(ert-deftest riven/update-packages-modifiers ()
  "Modifier flags turn the corresponding options on or off."
  (let ((opts (riven/update-packages--parse-args
               '("--dry-run" "--elpa-only" "--no-refresh" "--no-fetch"
                 "--allow-build"))))
    (should (plist-get opts :dry-run))
    (should (plist-get opts :elpa))
    (should-not (plist-get opts :vc))
    (should-not (plist-get opts :refresh))
    (should-not (plist-get opts :fetch))
    (should (plist-get opts :allow-build))))

(ert-deftest riven/update-packages-only-repeats ()
  "`--only' is repeatable and accumulates package symbols."
  (let ((opts (riven/update-packages--parse-args
               '("--only" "vertico" "--vc-only" "--only" "corfu"))))
    (should (equal (plist-get opts :only) '(vertico corfu)))
    (should-not (plist-get opts :elpa))))

(ert-deftest riven/update-packages-rejects-unknown-option ()
  "Unknown options are a usage error."
  (should-error (riven/update-packages--parse-args '("--bogus"))
                :type 'user-error))

(ert-deftest riven/update-packages-rejects-conflicting-scope ()
  "`--elpa-only' together with `--vc-only' leaves nothing to do."
  (should-error (riven/update-packages--parse-args
                 '("--elpa-only" "--vc-only"))
                :type 'user-error))

(ert-deftest riven/update-packages-only-requires-name ()
  "`--only' without a name is a usage error."
  (should-error (riven/update-packages--parse-args '("--only"))
                :type 'user-error))

(provide 'test-update-packages)
;;; test-update-packages.el ends here
