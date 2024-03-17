;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Should be configured in per-project basis, good documentation at:
;; github.com/cjohansson/emacs-ssh-deploy#deployment-configuration-examples

;; Code:

(use-package ssh-deploy
  :straight t
  :hook ((after-save . ssh-deploy-after-save)
         (find-file . ssh-deploy-find-file))
  :init
  (+map! "od" '(ssh-deploy-hydra/body :wk "ssh-deploy"))
  :config
  (ssh-deploy-hydra "C-c C-z"))

(use-package tldr
  :straight t
  :init
  (+map! "h?" #'tldr)
  :custom
  (tldr-enabled-categories '("common" "linux" "osx")))


(defun toggle-vterm ()
  (interactive)
  (if (get-buffer-window "*vterm*")
      ;; (let ((kill-buffer-query-functions nil))
      ;;   (kill-buffer "*vterm*"))
      (delete-window (get-buffer-window "*vterm*"))
    (vterm)))

(use-package vterm
  :straight t
  :init
  (+map!
    "ot" #'toggle-vterm)
  ;; Hide vterm install window
  (add-to-list
   'display-buffer-alist
   `(" \\*Install vterm\\*"
     (display-buffer-no-window)
     (allow-no-window . t)))
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 5000)
  (vterm-tramp-shells '(("docker" "/bin/bash")))
  :config
  (define-key vterm-mode-map [return] #'vterm-send-return)
  ;; Once vterm is dead, the vterm buffer is useless. Why keep it around? We can
  ;; spawn another if want one.
  (setq vterm-kill-buffer-on-exit t)
  ;; 5000 lines of scrollback, instead of 1000
  (setq vterm-max-scrollback 5000)
  (setq-hook! 'vterm-mode-hook
    ;; Don't prompt about dying processes when killing vterm
    confirm-kill-processes nil
    ;; Prevent premature horizontal scrolling
    hscroll-margin 0))

;; (use-package multi-vterm
;;   :straight t
;;   :init
;;   (+map!
;;     "oT" #'multi-vterm-project)
;;   ;; Show at buttom
;;   (add-to-list
;;    'display-buffer-alist
;;    `("\\*vterminal - .*\\*" ;; multi-vterm-project / dedicated
;;      (display-buffer-reuse-window display-buffer-in-direction)
;;      (direction . bottom)
;;      (dedicated . t)
;;      (reusable-frames . visible)
;;      (window-height . 0.3)))
;;   :custom
;;   (multi-vterm-dedicated-window-height-percent 30))

(use-package docker
  :straight t
  :init
  (+map! "ok" #'docker))

(use-package docker-compose-mode
  :straight t)

;; Emacs 29 comes with `dockerfile-ts-mode'
(use-package dockerfile-ts-mode
  :straight (:type built-in)
  :mode "/Dockerfile\\'")
(use-package dockerfile-mode
  :straight t)

(use-package systemd
  :straight t)

(use-package pkgbuild-mode
  :straight t
  :config
  (+map-local! :keymaps 'pkgbuild-mode-map
    "b" #'pkgbuild-makepkg
    "a" #'pkgbuild-tar
    "r" #'pkgbuild-increase-release-tag
    "u" #'pkgbuild-browse-url
    "m" #'pkgbuild-update-sums-line
    "s" #'pkgbuild-update-srcinfo
    "e" #'pkgbuild-etags))

(use-package journalctl-mode
  :straight t
  :config
  (+map-local! :keymaps 'journalctl-mode-map
    "J" #'journalctl-next-chunk
    "K" #'journalctl-previous-chunk))

(use-package logview
  :straight t
  :init
  (+map! :infix "t"
    "l" '(log-view-mode :wk "+logview-mode")))

(use-package quickrun
  :straight t
  :commands (quickrun)
  :bind ("<f5>" . quickrun)
  :init
  (+map! :infix "o"
    "q" '(quickrun :wk "Quick run")))

(use-package devdocs
  :straight t
  :commands (devdocs-lookup)
  :config
  (add-hook 'python-mode-hook
          (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  (add-hook 'typescript-mode-hook
          (lambda () (setq-local devdocs-current-docs '("javascript"))))
  (add-hook 'c-mode-hook
            (lambda () (setq-local devdocs-current-docs '("c"))))
  (add-hook 'c++-mode-hook
            (lambda () (setq-local devdocs-current-docs '("c"))))
  (add-hook 'rust-mode-hook
            (lambda () (setq-local devdocs-current-docs '("rust"))))
  )


(use-package go-translate
  :straight (:host github :repo "lorniu/go-translate")
  :commands +gts-yank-translated-region +gts-translate-with
  :init
  (+map! :infix "o"
    "x" `(,(+cmdfy! (+gts-translate-with)) :wk "Translate with Google")
    "X" #'gts-do-translate)
  :custom
  ;; Your languages pairs
  (gts-translate-list '(("en" "zh")
                        ("zh" "en")))
  :config
  ;; Config the default translator, which will be used by the command `gts-do-translate'
  (setq gts-default-translator
        (gts-translator
         ;; Used to pick source text, from, to. choose one.
         :picker (gts-prompt-picker)
         ;; One or more engines, provide a parser to give different output.
         :engines (gts-google-engine :parser (gts-google-summary-parser))
         ;; Render, only one, used to consumer the output result.
         :render (gts-buffer-render)))

  ;; Custom texter which remove newlines in the same paragraph
  (defclass +gts-translate-paragraph (gts-texter) ())

  (cl-defmethod gts-text ((_ +gts-translate-paragraph))
    (when (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (let ((case-fold-search nil))
            (while (re-search-forward "\n[^\n]" nil t)
              (replace-region-contents
               (- (point) 2) (- (point) 1)
               (lambda (&optional a b) " ")))
            (buffer-string))))))

  ;; Custom picker to use the paragraph texter
  (defclass +gts-paragraph-picker (gts-picker)
    ((texter :initarg :texter :initform (+gts-translate-paragraph))))

  (cl-defmethod gts-pick ((o +gts-paragraph-picker))
    (let ((text (gts-text (oref o texter))))
      (when (or (null text) (zerop (length text)))
        (user-error "Make sure there is any word at point, or selection exists"))
      (let ((path (gts-path o text)))
        (setq gts-picker-current-path path)
        (cl-values text path))))

  (defun +gts-yank-translated-region ()
    (interactive)
    (gts-translate
     (gts-translator
      :picker (+gts-paragraph-picker)
      :engines (gts-google-engine)
      :render (gts-kill-ring-render))))

  (defun +gts-translate-with (&optional engine)
    (interactive)
    (gts-translate
     (gts-translator
      :picker (+gts-paragraph-picker)
      :engines
      (cond ((eq engine 'deepl)
             (gts-deepl-engine
              :auth-key ;; Get API key from ~/.authinfo.gpg (machine api-free.deepl.com)
              (funcall
               (plist-get (car (auth-source-search :host "api-free.deepl.com" :max 1))
                          :secret))
              :pro nil))
            ((eq engine 'bing) (gts-bing-engine))
            (t (gts-google-engine)))
      :render (gts-buffer-render)))))


(use-package restclient
  :straight t
  :defer t
  :mode (("\\.http\\'" . restclient-mode)))


(use-package elfeed
  :straight t
  :defer t
  :commands (elfeed)
  :config
  (setq elfeed-feeds
      '(("http://nullprogram.com/feed/" blog emacs)
        "http://www.50ply.com/atom.xml"  ; no autotagging
        ("http://nedroid.com/feed/" webcomic))))


(provide 're-tools)
