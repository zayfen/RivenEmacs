;;; re-latex.el --- LaTeX related stuff -*- lexical-binding: t; -*-




;; Adapted from Doom Emacs
(use-package tex
  :straight auctex
  :hook ((tex-mode TeX-mode latex-mode LaTeX-mode) . TeX-source-correlate-mode)
  :custom
  (TeX-parse-self t) ; parse on load
  (TeX-auto-save t)  ; parse on save
  (TeX-auto-local ".auctex-auto") ; use hidden directories for AUCTeX files.
  (TeX-style-local ".auctex-style")
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-start-server nil) ; don't start the Emacs server when correlating sources.
  (TeX-electric-sub-and-superscript t) ; automatically insert braces after sub/superscript in `LaTeX-math-mode'.
  (TeX-save-query nil) ; just save, don't ask before each compilation.
  :init
  (+map-local! :keymaps '(tex-mode-map TeX-mode-map latex-mode-map LaTeX-mode-map)
    "c" #'TeX-command-run-all
    "m" #'TeX-command-master
    "e" #'TeX-engine-set
    "v" #'TeX-view)
  :config
  (when (functionp 'pdf-tools-install)
    (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))))

;; Adapted from Doom Emacs
(use-package auctex-latexmk
  :straight t
  :after latex
  :demand t
  :hook (LaTeX-mode . +tex--set-latexmk-as-default-cmd-h)
  :defines +tex--set-latexmk-as-default-cmd-h
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (defun +tex--set-latexmk-as-default-cmd-h ()
    (setq TeX-command-default "LatexMk"))
  ;; Add LatexMk as a TeX target.
  (auctex-latexmk-setup))

(use-package bibtex
  :straight (:type built-in)
  :hook (bibtex-mode . display-line-numbers-mode)
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-align-at-equal-sign t)
  (bibtex-text-indentation 20)
  :config
  (+map-local! :keymaps 'bibtex-mode-map
    "l" #'bibtex-fill-entry
    "r" #'bibtex-reformat))

;; Inspired by Doom Emacs
(use-package reftex
  :straight (:type built-in)
  :hook (LaTeX-mode . turn-on-reftex)
  :hook (reftex-toc-mode . reftex-toc-rescan)
  :custom
  ;; Get RefTeX working with BibLaTeX. See: tex.stackexchange.com/a/31992/43165
  (reftex-cite-format
   '((?a . "\\autocite[]{%l}")
     (?b . "\\blockcquote[]{%l}{}")
     (?c . "\\cite[]{%l}")
     (?f . "\\footcite[]{%l}")
     (?n . "\\nocite{%l}")
     (?p . "\\parencite[]{%l}")
     (?s . "\\smartcite[]{%l}")
     (?t . "\\textcite[]{%l}"))
   ;; This is needed when `reftex-cite-format' is set. See:
   ;; superuser.com/a/1386206
   (LaTeX-reftex-cite-format-auto-activate nil)
   (reftex-plug-into-AUCTeX t)
   (reftex-toc-split-windows-fraction 0.3))
  :config
  (+map-local! :keymaps 'reftex-mode-map
    ";" 'reftex-toc)
  (+nvmap! :keymaps 'reflex-toc-mode-map
    "j"   #'next-line
    "k"   #'previous-line
    "q"   #'kill-buffer-and-window
    "ESC" #'kill-buffer-and-window)
  (with-eval-after-load 'evil
    (add-hook 'reftex-mode-hook #'evil-normalize-keymaps)))


(provide 're-latex)
