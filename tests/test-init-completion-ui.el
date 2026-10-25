;;; test-init-completion-ui.el --- Tests for in-buffer completion -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression coverage for the Corfu popup-info setup: the documentation
;; popup must be loaded and globally enabled when completion is used, and
;; `M-n'/`M-p' must scroll that popup while `C-n'/`C-p' keep switching
;; candidates.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst riven/test-init-completion-ui-repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the RivenEmacs repository.")

(let ((package-user-dir
       (expand-file-name "elpa" riven/test-init-completion-ui-repo-root)))
  (when (file-directory-p package-user-dir)
    (package-initialize)))

(condition-case nil
    (progn
      (require 'init-minibuffer)
      (require 'init-completion-ui))
  (error nil))

(ert-deftest riven/init-completion-ui-popupinfo-is-active ()
  "Corfu popup documentation is loaded, enabled, and bound to M-h."
  (skip-unless (featurep 'init-completion-ui))
  (skip-unless (featurep 'corfu-popupinfo))
  (should (bound-and-true-p corfu-popupinfo-mode))
  (should (equal corfu-popupinfo-delay '(0.25 . 0.1)))
  (should (eq (lookup-key corfu-map (kbd "M-h"))
              #'corfu-popupinfo-documentation)))

(ert-deftest riven/init-completion-ui-doc-scroll-bindings ()
  "M-n/M-p scroll the documentation popup; C-n/C-p keep switching candidates."
  (skip-unless (featurep 'init-completion-ui))
  (should (eq (lookup-key corfu-map (kbd "M-n"))
              #'riven/corfu-scroll-doc-forward))
  (should (eq (lookup-key corfu-map (kbd "M-p"))
              #'riven/corfu-scroll-doc-backward))
  ;; C-n/C-p remain candidate navigation through Corfu's remaps.
  (should (eq (lookup-key corfu-map [remap next-line]) #'corfu-next))
  (should (eq (lookup-key corfu-map [remap previous-line]) #'corfu-previous)))

(ert-deftest riven/corfu-scroll-doc-forward-scrolls-when-visible ()
  "`M-n' scrolls the documentation popup when it is visible."
  (skip-unless (featurep 'init-completion-ui))
  (let (scrolled)
    (cl-letf (((symbol-function 'riven/corfu-doc-visible-p) (lambda () t))
              ((symbol-function 'corfu-popupinfo-scroll-up)
               (lambda (&optional n) (setq scrolled (or n 'page))))
              ((symbol-function 'corfu-next)
               (lambda (&optional _n) (ert-fail "should not switch candidate"))))
      (riven/corfu-scroll-doc-forward)
      (should scrolled))))

(ert-deftest riven/corfu-scroll-doc-forward-falls-back-to-next-candidate ()
  "`M-n' switches candidates when no documentation popup is visible."
  (skip-unless (featurep 'init-completion-ui))
  (let (next-called)
    (cl-letf (((symbol-function 'riven/corfu-doc-visible-p) (lambda () nil))
              ((symbol-function 'corfu-next)
               (lambda (&optional _n) (setq next-called t))))
      (riven/corfu-scroll-doc-forward)
      (should next-called))))

(ert-deftest riven/corfu-scroll-doc-backward-behaviour ()
  "`M-p' scrolls the popup backward or selects the previous candidate."
  (skip-unless (featurep 'init-completion-ui))
  (let (scrolled previous)
    (cl-letf (((symbol-function 'riven/corfu-doc-visible-p) (lambda () t))
              ((symbol-function 'corfu-popupinfo-scroll-down)
               (lambda (&optional _n) (setq scrolled t)))
              ((symbol-function 'corfu-previous)
               (lambda (&optional _n) (setq previous t))))
      (riven/corfu-scroll-doc-backward)
      (should scrolled)
      (should-not previous))
    (setq scrolled nil previous nil)
    (cl-letf (((symbol-function 'riven/corfu-doc-visible-p) (lambda () nil))
              ((symbol-function 'corfu-popupinfo-scroll-down)
               (lambda (&optional _n) (setq scrolled t)))
              ((symbol-function 'corfu-previous)
               (lambda (&optional _n) (setq previous t))))
      (riven/corfu-scroll-doc-backward)
      (should previous)
      (should-not scrolled))))

;;; test-init-completion-ui.el ends here
