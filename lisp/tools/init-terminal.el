;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-terminal.el --- Terminal

(defcustom rivenEmacs-ghostel-font-family nil
  "Font family used for Ghostel terminal buffers.
When nil, automatically use the first installed terminal-oriented CJK
font (Sarasa Term SC Nerd and friends).  Using one font for both Latin
and CJK keeps Ghostel's two-column CJK cells and ascent/descent metrics
consistent, avoiding its glyph-shrinking workaround.  Set this to nil
to keep the automatic choice, or to a specific family such as
\"JetBrainsMono Nerd Font\" if you prefer that Latin font."
  :type '(choice (const :tag "Auto terminal CJK font" nil) string)
  :group 'rivenEmacs)

(defvar riven/ghostel--configured-font nil
  "Ghostel font family selected by `riven/ghostel-configure-font'.")

(defun riven/ghostel--available-terminal-font ()
  "Return the first installed terminal-oriented CJK font family."
  (catch 'found
    (dolist (family '("Sarasa Term SC Nerd"
                      "Sarasa Term SC"
                      "Sarasa Mono SC"
                      "Sarasa Fixed SC"
                      "Noto Sans Mono CJK SC"))
      (when (find-font (font-spec :name family))
        (throw 'found family)))))

(defun riven/ghostel-configure-font ()
  "Use a terminal-metric font for Ghostel buffers.
This keeps Latin and CJK glyphs from the same family so Ghostel does
not shrink CJK fallback glyphs to fit the Latin cell.  The selected
family can be overridden with `rivenEmacs-ghostel-font-family'."
  (interactive)
  (let* ((size (if (fboundp 'riven/font-size-default)
                   (riven/font-size-default)
                 16))
         (family (or (and rivenEmacs-ghostel-font-family
                          (find-font (font-spec :name rivenEmacs-ghostel-font-family))
                          rivenEmacs-ghostel-font-family)
                     (riven/ghostel--available-terminal-font))))
    (setq riven/ghostel--configured-font family)
    (when family
      (set-face-attribute 'ghostel-default nil
                          :font (format "%s-%s" family size))
      (message "Ghostel: using %s-%s for terminal text" family size))))

(defun riven/ghostel--mode-setup ()
  "Prevent Ghostel from shrinking CJK glyphs in the current buffer."
  (when (and riven/ghostel--configured-font
             (boundp 'ghostel-glyph-scale-floor))
    (setq-local ghostel-glyph-scale-floor 1.0)))

(use-package ghostel
  :vc (:url "https://github.com/dakra/ghostel"
       :lisp-dir "lisp"
       :rev :newest)
  :commands (ghostel ghostel-other-window)
  :config
  (riven/ghostel-configure-font)
  (add-hook 'ghostel-mode-hook #'riven/ghostel--mode-setup))

(use-package eat
  :vc (:url "https://codeberg.org/akib/emacs-eat")
  :commands (eat eat-line-mode eat-semi-char-mode)
  :hook (eat-exec-hook . (lambda (&rest _) (eat-line-mode))))


(add-hook 'eshell-load-hook #'eat-eshell-mode)

(use-package vterm
  :commands (vterm vterm-other-window)
  :custom
  (vterm-shell (cond
                 ((file-exists-p "/opt/homebrew/bin/zsh") "/opt/homebrew/bin/zsh")
                 ((file-exists-p "/usr/local/bin/zsh") "/usr/local/bin/zsh")
                 ((file-exists-p "/usr/bin/zsh") "/usr/bin/zsh")
                 ((file-exists-p "/bin/bash") "/bin/bash")
                 ((file-exists-p "/bin/sh") "/bin/sh")
                 (t "zsh")))  ; Set your default shell (with fallback)
  (vterm-max-scrollback 10000)  ; Increase scrollback buffer
  (vterm-kill-buffer-on-exit t)  ; Auto-kill buffer on exit
  ;; :bind
  ;; (:map vterm-mode-map
  ;;       ("C-c C-c" . vterm-send-C-c)  ; Send C-c to terminal
  ;;       ("C-c C-y" . vterm-yank)  ; Yank from kill-ring
  ;;       ("C-c C-t" . vterm-copy-mode))  ; Toggle copy mode
)

;; ;; Optional: vterm-toggle package for better toggling
;; (use-package vterm-toggle
;;   :ensure t
;;   :after vterm
;;   :custom
;;   (vterm-toggle-scope 'project)  ; 'project, 'frame, or 'nil
;;   (vterm-toggle-fullscreen-p nil)
;;   (vterm-toggle-reset-window-configration-after-exit t))

(provide 'init-terminal)
;;; init-terminal.el ends here.
