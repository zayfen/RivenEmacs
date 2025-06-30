;; -*- coding: utf-8; lexical-binding: t; -*-


(use-package general
  :vc (:url "https://github.com/noctuid/general.el")
  :config

  (defun unbind-prefixed-keys (prefix &optional keymap)
    "Unbind all keys in the keymap bound at PREFIX in KEYMAP (default: `global-map`)."
    (let* ((map (or keymap global-map))
           (prefix-kbd (kbd prefix))
           (submap (lookup-key map prefix-kbd)))
      (when (keymapp submap)
        (map-keymap
         (lambda (key _binding)
           (let ((full-key (vconcat prefix-kbd (vector key))))
             (define-key map full-key nil)))
         submap))))

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
