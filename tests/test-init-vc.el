;;; test-init-vc.el --- Tests for init-vc -*- lexical-binding: t; -*-

;;; Commentary:
;; Guard the merge-conflict ergonomics: conflicted files must enable
;; smerge-mode automatically, and the resolution hydra must stay reachable.

;;; Code:

(require 'ert)
(require 'seq)

(defconst riven/test-init-vc-repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the RivenEmacs repository.")

(defun riven/test-init-vc-read-forms ()
  "Return all top-level forms from init-vc.el."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "lisp/vcs/init-vc.el" riven/test-init-vc-repo-root))
    (let (forms)
      (condition-case nil
          (while t
            (push (read (current-buffer)) forms))
        (end-of-file (nreverse forms))))))

(defun riven/test-init-vc-find (tree predicate)
  "Return the first element of TREE matching PREDICATE, or nil."
  (cond
   ((funcall predicate tree) tree)
   ((consp tree)
    (or (riven/test-init-vc-find (car tree) predicate)
        (riven/test-init-vc-find (cdr tree) predicate)))))

(ert-deftest riven/init-vc-auto-enables-smerge-mode ()
  "Conflict-marker buffers enable smerge-mode without manual setup."
  (let ((forms (riven/test-init-vc-read-forms)))
    (should (seq-some
             (lambda (form)
               (and (consp form)
                    (eq (car form) 'defun)
                    (eq (cadr form) 'riven/smerge-mode-maybe-enable)))
             forms))
    (should (member '(add-hook 'find-file-hook
                               #'riven/smerge-mode-maybe-enable)
                    forms))
    (should (member '(add-hook 'after-revert-hook
                               #'riven/smerge-mode-maybe-enable)
                    forms))))

(ert-deftest riven/init-vc-smerge-hydra-has-resolve-all ()
  "The smerge hydra exposes `smerge-resolve-all' on A."
  (let ((forms (riven/test-init-vc-read-forms)))
    (should (riven/test-init-vc-find
             forms
             (lambda (node)
               (and (consp node)
                    (equal (car node) "A")
                    (eq (cadr node) 'smerge-resolve-all)))))))

;;; test-init-vc.el ends here
