(use-package auto-save
  :ensure t
  :straight (auto-save
             :type git
             :host github
             :repo "manateelazycat/auto-save"
             :files ("*" (:exclude ".git"))
             :build nil)
  :init
  (add-to-list 'load-path (straight--repos-dir "auto-save"))
  :config
  (auto-save-enable)
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t)
  (setq auto-save-disable-predicates
      '((lambda ()
      (string-suffix-p
      "gpg"
      (file-name-extension (buffer-name)) t)))))
