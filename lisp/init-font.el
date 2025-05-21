;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-font.el --- font settings

;;; Commentary:
;; set default font

;;; Code:

(set-frame-font (format "%s-%s"
                        "Liga SFMono Nerd Font"
                        (cond
                         ((eq system-type 'gnu/linux) 12)
                         ((eq system-type 'windows-nt) 12)
                         ((eq system-type 'darwin) 14))))

(set-face-attribute 'fixed-pitch nil
                    :font (font-spec :family "Liga SFMono Nerd Font"
                                     :weight 'regular
                                     :size (cond ((eq system-type 'gnu/linux) 12)
                                                 ((eq system-type 'windows-nt) 12)
                                                 ((eq system-type 'darwin) 14))))

(set-face-attribute 'variable-pitch nil
                    :font (font-spec :family "SF Pro Display"
                                     :weight 'regular
                                     :size (cond ((eq system-type 'gnu/linux) 12)
                                                 ((eq system-type 'windows-nt) 12)
                                                 ((eq system-type 'darwin) 14))))


(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))


(when (display-graphic-p)
  (cl-loop for fontname in '("Apple Color Emoji" "Segoe UI Emoji" "Noto Color Emoji")
           when (font-installed-p fontname)
           return (set-fontset-font t 'unicode
                                    (font-spec :family fontname
                                               :size (cond ((eq system-type 'gnu/linux) 12)
                                                           ((eq system-type 'windows-nt) 12)
                                                           ((eq system-type 'darwin) 14)))
                                    nil 'prepend))

  (cl-loop for fontname in '("Sarasa Mono SC" "PingFang SC" "Microsoft YaHei" "Source Han Sans SC" "WenQuanYi Micro Hei")
           when (font-installed-p fontname)
           return (dolist (charset '(kana han symbol cjk-misc bopomofo))
                      (set-fontset-font (frame-parameter nil 'font) charset
                                        (font-spec :family fontname
                                                   :height (cond ((eq system-type 'gnu/linux) 12)
                                                                 ((eq system-type 'windows-nt) 12)
                                                                 ((eq system-type 'darwin)  12))))))

  (cl-loop for fontname in '("HanaMinB" "SimSun-ExtB")
           when (font-installed-p fontname)
           return (set-fontset-font (frame-parameter nil 'font) '(#x20000 . #x2A6DF)
                                    (font-spec :name fontname
                                               :weight 'normal
                                               :slant 'normal
                                               :size (cond ((eq system-type 'gnu/linux) 12)
                                                           ((eq system-type 'windows-nt) 12)
                                                           ((eq system-type 'darwin) 14)))))

                                                           )

(provide 'init-font)
