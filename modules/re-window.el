;;; re-window.el --- Windows and frames -*- lexical-binding: t; -*-




;; Window configuration for special windows.
;; This section inspired by the article "Demystifying Emacs’s Window Manager" found here:
;; www.masteringemacs.org/article/demystifying-emacs-window-manager
(+deferred!
 (add-to-list
  'display-buffer-alist
  `(,(rx (seq "*" (or "Help" (seq "helpful" (zero-or-more not-newline))) "*"))
    (display-buffer-reuse-window display-buffer-pop-up-window)
    (inhibit-same-window . t)))

 ;; Show *Warnings* at bottom
 (add-to-list
  'display-buffer-alist
  `("*Warnings*"
    (display-buffer-reuse-window display-buffer-in-direction)
    (direction . bottom) ;; bottom (above below...)
    (dedicated . t) ;; Close when finished
    (reusable-frames . visible) ;;
    (window-height . 10)))

 ;; Show dictionary definition and completion buffer on the right side
 (add-to-list
  'display-buffer-alist
  `(,(rx (seq "*" (or "Dictionary" "lexic" "Completions") "*"))
    (display-buffer-in-side-window)
    (side . right)
    (window-width . 82)))

 (add-to-list
  'display-buffer-alist
  `(,(rx (seq "*" (or "eshell" "vterm" "terminal") "*"))
    ;; (display-buffer-reuse-window display-buffer-at-bottom)
    (display-buffer-reuse-window display-buffer-in-direction)
    (direction . bottom) ;; bottom (above below...)
    (dedicated . t) ;; Close when finished
    (reusable-frames . visible) ;;
    (window-height . 0.3)))


 ;; Adapted from: github.com/Phundrak/dotfiles/blob/master/org/config/emacs.org
 (with-eval-after-load 'hydra
   (defhydra +window-adjust-size (:hint nil :foreign-keys warn)
     "
^Zoom^                                ^Other
^^^^^^^-----------------------------------------
[_t_/_s_] shrink/enlarge vertically   [_q_] quit
[_c_/_r_] shrink/enlarge horizontally
"
     ("q" nil :exit t)
     ("c" shrink-window-horizontally)
     ("t" enlarge-window)
     ("s" shrink-window)
     ("r" enlarge-window-horizontally))

   (+map!
     :infix "w"
     "a" '(+window-adjust-size/body :wk "Adjust window size"))))


;; ace-window
(use-package ace-window
  :ensure t
  :defer t
  :init

  (progn
    (global-set-key [remap other-window] 'ace-window)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(set-frame-parameter (selected-frame)
                     'internal-border-width 0)
(provide 're-window)