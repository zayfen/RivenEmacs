;; modules/langs/re-python.el -*- lexical-binding: t; -*-

(use-package python
  :mode ("\\.py\\'" . python-ts-mode)
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'" . conf-mode))


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


(provide 'python)
