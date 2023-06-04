;;; re-popup.el --- Config for popup -*- lexical-binding: t; -*-


(use-package eldoc
  :straight (:type built-in)
  :custom
  (eldoc-documentation-strategy #'eldoc-documentation-compose))

(use-package eldoc-box
  :straight t
  :hook (prog-mode . eldoc-box-hover-at-point-mode)
  :hook ((tab-bar-mode tool-bar-mode) . +eldoc-box-hover-at-point-fix-h)
  :config
  ;; HACK: Temporary fix for `eldoc-box-hover-at-point-mode' with `tab-bar-mode'
  ;; and `tool-bar-mode'.
  (defun +eldoc-box-hover-at-point-fix-h ()
    (when (bound-and-true-p eldoc-box-hover-at-point-mode)
      (eldoc-box-hover-at-point-mode -1)
      (eldoc-box-hover-at-point-mode 1))))




(provide 're-popup)
