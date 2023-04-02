;; stolen from Doom emacs
(defun +javascript-add-npm-path-h ()
  "Add node_modules/.bin to `exec-path'."
  (when-let ((node-modules-parent (locate-dominating-file default-directory "node_modules/"))
             (node-modules-dir (expand-file-name "node_modules/.bin/" node-modules-parent)))
    (make-local-variable 'exec-path)
    (add-to-list 'exec-path node-modules-dir)
    (message ":lang:javascript: add %s to $PATH" (expand-file-name "node_modules/" node-modules-parent))))


(provide 're-defuns)
