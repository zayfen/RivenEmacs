;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


;; PERF+HACK: At some point, RivenEmacs startup become too slow, specially when
;; initializing `general' and `evil'. After trying several configurations, I
;; figured out that deferring `general' solves the issue. However, deferring
;; `general' means that we cannot define the keybindings when loading other
;; packages, i.e. before `general' gets loaded and the RivenEmacs definers (i.e.
;; `+rivenemacs--internal-map!', `+rivenemacs--internal-map-local!', ...) are made
;; available. We overcome this by defining these macros to define the
;; keybindings by wrapping the actual definition in a `with-eval-after-load'
;; block to be evaluated only after `general' gets loaded and configured and the
;; definers are ready (See `re-keybindings').
;;;###autoload
(defmacro +map! (&rest args)
  "A wrapper around `+rivenemacs--internal-map!'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 're-general-ready
    (+rivenemacs--internal-map! ,@args)))

;;;###autoload
(defmacro +map-local! (&rest args)
  "A wrapper around `+rivenemacs--internal-map-local!'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 're-general-ready
    (+rivenemacs--internal-map-local! ,@args)))


(defmacro +map-executor! (&rest args)
  "A wrapper around `+rivenemacs--internal-map!'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 're-general-ready
    (+rivenemacs--internal-map! ,@args)))


;; Wrappers around `general's VIM like definers, needs `general-evil-setup' to
;; be executed (See `re-keybindings')
;;;###autoload
(defmacro +nmap! (&rest args)
  "A wrapper around `general-nmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 're-general-ready
    (general-emap ,@args)))

;;;###autoload
(defmacro +vmap! (&rest args)
  "A wrapper around `general-vmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 're-general-ready
    (general-emap ,@args)))

;;;###autoload
(defmacro +mmap! (&rest args)
  "A wrapper around `general-mmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 're-general-ready
    (general-emap ,@args)))

;;;###autoload
(defmacro +imap! (&rest args)
  "A wrapper around `general-imap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 're-general-ready
    (general-emap ,@args)))

;;;###autoload
(defmacro +emap! (&rest args)
  "A wrapper around `general-emap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 're-general-ready
    (general-emap ,@args)))

;;;###autoload
(defmacro +omap! (&rest args)
  "A wrapper around `general-omap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 're-general-ready
    (general-emap ,@args)))

;;;###autoload
(defmacro +rmap! (&rest args)
  "A wrapper around `general-rmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 're-general-ready
    (general-emap ,@args)))

;;;###autoload
(defmacro +iemap! (&rest args)
  "A wrapper around `general-iemap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 're-general-ready
    (general-emap ,@args)))

;;;###autoload
(defmacro +nvmap! (&rest args)
  "A wrapper around `general-nvmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 're-general-ready
    (general-emap ,@args)))
