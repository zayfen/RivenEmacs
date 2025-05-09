;; -*- coding: utf-8; lexical-binding: t -*-

(require 'url)

(defun get-project-root ()
  (if (fboundp 'projectile-project-root)
      (projectile-project-root)
    (project-root (project-current))))

;; Ripgrep the current word from project root
(defun consult-ripgrep-at-point ()
  (interactive)
  (consult-ripgrep (get-project-root) (thing-at-point 'symbol)))

;; Ripgrep the selected region from project root
(defun consult-ripgrep-region ()
  (interactive)
  (consult-ripgrep (get-project-root) (buffer-substring-no-properties (region-beginning) (region-end))))

;; enhance ripgrep
(defun consult-ripgrep-ex ()
  (interactive)
  (if (use-region-p)
      (consult-ripgrep-region)
    (consult-ripgrep-at-point)))

(defun my-select-inside-quotes ()
  "grab text between double straight quotes on each side of cursor."
  (interactive)
  (let (p1 p2)
    (skip-chars-backward "^([~@\"'<])" (line-beginning-position))
    (setq p1 (point))
    (skip-chars-forward "^([\"']>)" (line-end-position))
    (setq p2 (point))
    (buffer-substring-no-properties p1 p2)))


(defun +goto-file-at-point ()
  "Find the file at point and open it."
  (interactive)
  (let (file-path)
    (setq file-path (my-select-inside-quotes))
    (consult-fd (get-project-root) file-path)
    ))


(defun +remove-invalidate-buffers ()
  "Remove invalidate buffers"
  (interactive)
  (mapc (lambda (buf)
          (with-current-buffer buf
            (if (and (buffer-file-name buf)
                     (not (file-exists-p (buffer-file-name buf)))
                     (not (buffer-modified-p buf)))
                (kill-buffer buf))))
        (buffer-list)))


(defun consult-line-ex ()
  (interactive)
  (if (use-region-p)
      (consult-line (buffer-substring-no-properties (region-beginning) (region-end)))
    (consult-line)))

(defun +consult-fd-in-home ()
  "Find file in home directory."
  (interactive)
  (consult-fd "~/"))

(defun +open-in-system-explorer ()
  "Open the current directory in the system's file explorer."
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (w32-shell-execute "explorer" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((eq system-type 'darwin) (shell-command "open ."))
   ((eq system-type 'gnu/linux) (shell-command "xdg-open ."))))

(defun +open-term-in-current-directory ()
  "Open a terminal in the current directory."
  (interactive)
  (let ((default-directory (if (buffer-file-name)
                               (file-name-directory (buffer-file-name))
                             default-directory)))
    (ansi-term "/usr/bin/zsh")))

(defun +show-current-buffer-path ()
  "Show the absolute path of the current buffer in the minibuffer."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (if file-path
        (message "%s" (file-truename file-path))
      (message "Current buffer is not visiting a file."))))

(defun riven/google-search (keyword)
  "Search Google for the given KEYWORD."
  (interactive "sSearch Google for: ")
  (let* ((base-url "https://www.google.com/search?q=")
         (encoded-keyword (url-encode-url keyword))
         (search-url (concat base-url encoded-keyword)))
    (message "Searching Google for '%s'..." keyword)
    (browse-url search-url)
    ;; Clear the minibuffer and echo area
    (message "")))


(defun riven/google-translate (target-language)
  "Translate text (region or user input) to TARGET-LANGUAGE using Google Translate.
If a region is active, translate the text in the region.
Otherwise, prompt the user for the text to translate.
Google automatically detects the source language.
Prompts for the target language code (e.g., \"en\", \"es\", \"fr\").
Opens the translation in a browser."
  (interactive
   ;; Only prompt for the target language interactively.
   ;; The text to translate is determined inside the function.
   "sTarget language code (e.g., en, es, fr): ")

  (let* (
         ;; Determine the text to translate: use region if active, otherwise prompt
         (text-to-translate
          (if (region-active-p)
              ;; If a region is active, get the text from region-beginning to region-end
              (buffer-substring-no-properties (region-beginning) (region-end))
            ;; If no region is active, prompt the user for the text
            (read-string "Text to translate: ")))

         ;; Encode the text for use in the URL
         (encoded-text (url-encode-url text-to-translate))

         ;; Construct the complete Google Translate URL
         ;; Base URL + text parameter + source language (auto-detect) parameter + target language parameter
         (translate-url (concat "https://translate.google.com/?"
                                "text=" encoded-text
                                "&sl=auto" ;; Specify auto-detect source language
                                "&tl=" target-language))) ;; Use the target language from interactive prompt

    ;; Open the URL in the default web browser
    (browse-url translate-url)))


(provide 'init-helper)
