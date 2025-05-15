;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-font.el --- font settings

;;; Commentary:
;; set default font

;;; Code:


(set-face-attribute 'default nil
                   :font (font-spec :family "Liga SFMono Nerd Font"
                                    :weight 'regular
                                    :size (cond ((eq system-type 'gnu/linux) 13)
                                                ((eq system-type 'windows-nt) 12)
                                                ((eq system-type 'darwin) 14))))

(set-face-attribute 'fixed-pitch nil
                    :font (font-spec :family "Liga SFMono Nerd Font"
                                     :weight 'regular
                                     :size (cond ((eq system-type 'gnu/linux) 13)
                                                 ((eq system-type 'windows-nt) 12)
                                                 ((eq system-type 'darwin) 14))))

(set-face-attribute 'variable-pitch nil
                    :font (font-spec :family "SF Pro Display"
                                     :weight 'regular
                                     :size (cond ((eq system-type 'gnu/linux) 13)
                                                 ((eq system-type 'windows-nt) 12)
                                                 ((eq system-type 'darwin) 14))))
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(when (display-graphic-p)
  (cl-loop for font in '("Liga SFMono Nerd Font" "JetBrains Mono" "FiraCode Nerd Font" 
                       "Cascadia Code" "SF Mono" "Menlo" "Monaco" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute
                   'default nil
                   :font (font-spec :family font
                                    :weight 'normal
                                    :size (cond ((eq system-type 'gnu/linux) 20)
                                                ((eq system-type 'windows-nt) 12.5)
                                                ((eq system-type 'darwin) 16)))))

  (cl-loop for font in '("Apple Color Emoji" "Segoe UI Emoji" "Noto Color Emoji")
           when (font-installed-p font)
           return (set-fontset-font t 'unicode
                                    (font-spec :family font
                                               :size (cond ((eq system-type 'gnu/linux) 14)
                                                           ((eq system-type 'windows-nt) 15.0)
                                                           ((eq system-type 'darwin) 16.0)))
                                    nil 'prepend))

  (cl-loop for font in '("PingFang SC" "Microsoft YaHei" "Noto Sans CJK SC" 
                       "Source Han Sans SC" "WenQuanYi Micro Hei")
           when (font-installed-p font)
           return (set-fontset-font t 'han
                                   (font-spec :family font
                                              :weight 'regular
                                              :size (cond ((eq system-type 'gnu/linux) 15)
                                                          ((eq system-type 'windows-nt) 14)
                                                          ((eq system-type 'darwin) 15)))))
  (cl-loop for font in '("HanaMinB" "SimSun-ExtB")
           when (font-installed-p font)
           return (set-fontset-font t '(#x20000 . #x2A6DF)
                                    (font-spec :name font
                                               :weight 'normal
                                               :slant 'normal
                                               :size (cond ((eq system-type 'gnu/linux) 18)
                                                           ((eq system-type 'windows-nt) 15.0)
                                                           ((eq system-type 'darwin) 12.0))))))

(provide 'init-font)
