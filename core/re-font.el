;;; re-font.el --- font settings -*- lexical-binding: t; -*-

;;; Commentary:
;; set default font

;;; Code:


(set-face-attribute 'fixed-pitch nil :font (font-spec :family "Iosevka SS15"
                                                      :size (cond ((eq system-type 'gnu/linux) 12.5)
                                                                  ((eq system-type 'windows-nt) 12.5)
                                                                  ((eq system-type 'darwin) 18.0))))
(set-face-attribute 'variable-pitch nil :font (font-spec :family "Iosevka SS15"
                                                         :size (cond ((eq system-type 'gnu/linux) 12.5)
                                                                     ((eq system-type 'windows-nt) 12.5)
                                                                     ((eq system-type 'darwin) 18.0))))

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(when (display-graphic-p)
  (cl-loop for font in '("Menlo" "SF Mono" "Fira Code" "Cascadia Code" "Source Code Pro"
                         "Monaco" "Dejavu Sans Mono"
                         "Lucida Console" "Consolas" "SAS Monospace")
           when (font-installed-p font)
           return (set-face-attribute
                   'default nil
                   :font (font-spec :family font
                                    :weight 'normal
                                    :slant 'normal
                                    :size (cond ((eq system-type 'gnu/linux) 12.5)
                                                ((eq system-type 'windows-nt) 12.5)
                                                ((eq system-type 'darwin) 18)))))

  (cl-loop for font in '("OpenSansEmoji" "Noto Color Emoji" "Segoe UI Emoji"
                         "EmojiOne Color" "Apple Color Emoji" "Symbola" "Symbol")
           when (font-installed-p font)
           return (set-fontset-font t 'unicode
                                    (font-spec :family font
                                               :size (cond ((eq system-type 'gnu/linux) 16.5)
                                                           ((eq system-type 'windows-nt) 15.0)
                                                           ((eq system-type 'darwin) 18.0)))
                                    nil 'prepend))
  
  (cl-loop for font in '("思源黑体 CN" "思源宋体 CN" "微软雅黑 CN"
                         "Source Han Sans CN" "Source Han Serif CN"
                         "WenQuanYi Micro Hei" "文泉驿等宽微米黑"
                         "Microsoft Yahei UI" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff)
                                    (font-spec :name font
                                               :weight 'normal
                                               :slant 'normal
                                               :size (cond ((eq system-type 'gnu/linux) 12.8)
                                                           ((eq system-type 'windows-nt) 15.0)
                                                           ((eq system-type 'darwin) 12.5)
                                                           ))))

  (cl-loop for font in '("HanaMinB" "SimSun-ExtB")
           when (font-installed-p font)
           return (set-fontset-font t '(#x20000 . #x2A6DF)
                                    (font-spec :name font
                                               :weight 'normal
                                               :slant 'normal
                                               :size (cond ((eq system-type 'gnu/linux) 16.5)
                                                           ((eq system-type 'windows-nt) 15.0)
                                                           ((eq system-type 'darwin) 14.0))))))


(provide 're-font)
