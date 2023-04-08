;;; modules.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Abdelhak Bougouffa

;; This file can be used to override `rivenemacs-modules'
;; and `rivenemacs-core-modules'

;; Ordered list of enabled core modules
(setq rivenemacs-core-modules
      '(re-splash        ; Simple splash screen
        re-keybindings   ; Keybinding (general, which-key, hydra, ...)
        re-evil          ; Emacs as Vim (evil, evil-collection, evil-escape, evil-snipe, evil-numbers, ...)
        re-core-ui       ; Core UI (doom-themes, modus-themes, doom-modeline, ...)
        re-completion))  ; Completion (vertico, marginalia, corfu, cape, consult, embark, ...)

;; List of enabled modules
(setq rivenemacs-modules
      '(re-ui            ; User interface (focus, writeroom-mode, mixed-pitch, ...)
        re-editor        ; Editing (yasnippet, smartparens, unicode-fonts, ligature, ...)
        re-daemon        ; Emacs daemon tweaks
        re-undo          ; Better undoing (undo-fu, undo-fu-session, vundo, ...)
        re-multi-cursors ; Multi-cursors editing (iedit, evil-mc, evil-iedit-state, ...)
        re-vc            ; Version control (magit, forge, core-review, diff-hl, ...)
        re-project       ; Project management (project, consult-project-extra, ...)
        ;; obsolete/re-projectile ; Project management (projectile, consult-projectile, treemacs-projectile, ...)
        re-prog          ; Programming stuff (tree-sitter, eglot, eldoc, eldoc-box, apheleia, editorconfig, ...)
        re-checkers      ; Static checkers (flymake, flymake-easy, ...)
        ;; obsolete/re-flycheck   ; Static checkers (flycheck, ...)
        re-debug         ; Debugging tools (gdb-mi, realgud, disaster, ...)
        ;; re-lsp        ; LSP and DAP (lsp-mode, dap-mode, consult-lsp, lsp-pyright, ccls, ...)
        re-lisp          ; Lisps development (parinfer-rust, sly, macrostep, geiser, elisp, helpful, eros, ...)
        ;; re-embedded   ; Embedded systems (arduino, openocd, bitbake, vhdl-mode, ...)
        ;; re-robot      ; Robotics stuff (ros, robot-mode, ...)
        re-data          ; Data file formats (csv, yaml, toml, json, plantuml-mode, ...)
        ;; re-math       ; Mathematics (maxima, ess, ein, julia-mode, octave, ...)
        ;; re-modeling   ; Modeling tools (scad-mode, ...)
        re-org           ; Org-mode for life (org, org-contrib, org-modern, org-appear, ...)
        re-extra         ; Extra features (better-jumper, crux, ...)
        re-notes         ; Notes & Zettelkasten (org-roam, consult-org-roam, ...)
        re-eaf           ; EAF apps (browser, jupyter, file-sender, ...)
        ;; re-email      ; Email (mu4e, mu4e-alert, org-msg, ...)
        ;; re-rss        ; News feed (elfeed, ...)
        ;; re-lifestyle  ; *Very* opinionated lifestyle packages (awqat, ...)
        re-docs          ; Documents (pdf-tools, nov, ...)
        re-latex         ; LaTeX (auctex, auctex-latexmk, reftex, bibtex, ...)
        ;; re-biblio     ; Bibliography & citations (org-cite, citar, zotxt, ...)
        re-natural-langs ; Natural language stuff (spell-fu, go-translate, eglot-ltex, ...)
        re-files         ; Files and directories (dirvish, treemacs, vlf, ...)
        re-tools         ; System tools (tramp, vterm, tldr, ssh-deploy, docker, ...)
        re-tty           ; Emacs from terminal (xt-mouse, xclip, ...)
        re-fun           ; Games and funny packages (xkcd, speed-type, ...)
        re-media         ; Multimedia (empv, emms, ...)
        ;; re-workspaces ; Workspace separation (tabspaces, tab-bar, ...). NOTE: This is a WIP
        re-binary        ; Display binary files in hex or decompile them (hexl, ...) ...
        re-window))      ; Frame & window tweaks