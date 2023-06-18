;; modules/langs/re-python.el -*- lexical-binding: t; -*-

(use-package python
  :ensure t
  :mode (("\\.py\\'" . python-mode)
         ("[./]flake8\\'" . conf-mode)
         ("/Pipfile\\'" . conf-mode))
  )


(use-package pip-requirements
  :straight t)

(use-package pyvenv
  :straight t
  :after python
  :init
  (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
  (add-hook 'pyvenv-pre-activate-hooks #'+modeline-update-env-in-all-windows-h)
  :config
  (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)
  (add-to-list 'global-mode-string
               '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))
               'append))

(use-package poetry
  :straight t
  :after python
  :init
  (setq poetry-tracking-strategy 'switch-buffer))


(defun +python/optimize-imports ()
  "organize imports"
  (interactive)
  (pyimport-remove-unused)
  (py-isort-buffer))

(use-package pyimport
  :straight t
  :after python
  :init
  (+map-local! :keymaps 'python-ts-mode-map
    "i"  '(nil :wk "imports")
    "ii" #'pyimport-insert-missing
    "iR" #'pyimport-remove-unused
    "io" #'+python/optimize-imports))


(use-package py-isort
  :straight t
  :defer t
  :init
  (+map-local! :keymaps 'python-ts-mode-map
    "is" '(py-isort-buffer :wk "Sort imports")
    "ir" '(py-isort-region :wk "Sort region")))


(use-package nose
  :straight t
  :commands nose-mode
  :preface (defvar nose-mode-map (make-sparse-keymap))
  :mode ("/test_.+\\.py$" . nose-mode)
  :config
  (set-popup-rule! "^\\*nosetests" :size 0.4 :select nil)
  (set-yas-minor-mode! 'nose-mode)
  (+map-local! :keymaps 'nose-mode-map
    "t" '(nil :wk "nose test")
    "tr" #'nosetests-again
    "ta" #'nosetests-all
    "ts" #'nosetests-one
    "tv" #'nosetests-module
    "tA" #'nosetests-pdb-all
    "tO" #'nosetests-pdb-one
    "tV" #'nosetests-pdb-module))

(use-package python-pytest
  :straight t
  :commands python-pytest-dispatch
  :init
  (+map-local! :keymaps 'python-ts-mode-map
    "t" '(nil :wk "test")
    "ta" #'python-pytest
    "tf" #'python-pytest-file-dwim
    "tF" #'python-pytest-file
    "tt" #'python-pytest-function-dwim
    "tT" #'python-pytest-function
    "tr" #'python-pytest-repeat
    "tp" #'python-pytest-dispatch))

(flycheck-define-checker python-ruff
  "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
  :command ("ruff"
            "--format=text"
            (eval (when buffer-file-name
                    (concat "--stdin-filename=" buffer-file-name)))
            "-")
  :standard-input t
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  :modes (python-mode python-ts-mode))

;; Use something adapted to your config to add `python-ruff' to `flycheck-checkers'
;; This is an MVP example:
(setq python-mode-hook
      (list (defun my-python-hook ()
              (unless (bound-and-true-p org-src-mode)
                (when (buffer-file-name)
                  (setq-local flycheck-checkers '(python-ruff))
                  )))))

(setq python-ts-mode-hook
      (list (defun my-python-hook ()
              (unless (bound-and-true-p org-src-mode)
                (when (buffer-file-name)
                  (setq-local flycheck-checkers '(python-ruff))
                  )))))

(add-hook 'python-mode-hook  #'electric-pair-mode)
(add-hook 'python-ts-mode-hook  #'electric-pair-mode)

(provide 're-python)
