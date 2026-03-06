;; -*- coding: utf-8; lexical-binding: t; -*-

(eval-and-compile
  (let ((current-file (or load-file-name byte-compile-current-file buffer-file-name)))
    (when current-file
      (add-to-list 'load-path (file-name-directory current-file)))))


(use-package general
  :vc (:url "https://github.com/noctuid/general.el")
  :config

  (setq general-keymap-global-map global-map) ; Set the keymap for general.el as global-map
	(general-auto-unbind-keys t); Other configurations specific to your needs...

  (general-create-definer leader-def
    :prefix "C-c")

  (general-create-definer local-leader-def
    :prefix "C-c l")

  (general-create-definer query-leader-def
    :prefix "C-c q")

  (general-create-definer ai-leader-def
    :prefix "C-c a")

  (general-create-definer open-leader-def
    :prefix "C-c o")

  (general-create-definer navigate-leader-def
    :prefix "M-g"))


(provide 'init-general)
