;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-font.el --- font settings

;;; Code:

(defun riven/font-size-default ()
  "Return default font size for current platform."
  (cond
   ((eq system-type 'darwin) 16)
   ((eq system-type 'windows-nt) 12)
   (t 12)))

(defun riven/font-installed-p (font-name)
  "Check whether FONT-NAME exists on this system."
  (find-font (font-spec :name font-name)))

(defun riven/first-available-font (candidates)
  "Return first installed font from CANDIDATES."
  (seq-find #'riven/font-installed-p candidates))

(defun riven/apply-fonts ()
  "Apply fixed/variable/CJK/emoji font configuration consistently."
  (let* ((size (riven/font-size-default))
         (fixed (or (riven/first-available-font
                     '("Liga SFMono Nerd Font" "JetBrainsMono Nerd Font" "Menlo" "Monaco"))
                    "Monospace"))
         (variable (or (riven/first-available-font
                        '("SF Pro Display" "Inter" "Helvetica" "Arial"))
                       fixed))
         (emoji (riven/first-available-font
                 '("Apple Color Emoji" "Segoe UI Emoji" "Noto Color Emoji")))
         (cjk (riven/first-available-font
               '("Sarasa Mono SC" "PingFang SC" "Microsoft YaHei" "Source Han Sans SC" "WenQuanYi Micro Hei"))))
    (set-frame-font (format "%s-%s" fixed size) nil t)
    (set-face-attribute 'fixed-pitch nil :font fixed :weight 'regular :height (* 10 size))
    (set-face-attribute 'variable-pitch nil :font variable :weight 'regular :height (* 10 size))

    (when (and (display-graphic-p) emoji)
      (set-fontset-font t 'unicode
                        (font-spec :family emoji :size size)
                        nil 'prepend))

    (when (and (display-graphic-p) cjk)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family cjk))))))

(riven/apply-fonts)

(provide 'init-font)
