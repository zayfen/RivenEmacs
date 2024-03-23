;;; re-font.el --- font settings -*- lexical-binding: t; -*-

;;; Commentary:
;; set default font

(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas" t t)))
 ((string-equal system-type "darwin") ; macOS

  (when (member "Menlo" (font-family-list))
    (set-face-attribute 'default nil :font "Menlo" :height 180)
    (set-face-attribute 'fixed-pitch nil :font "Iosevka SS15")
    (set-face-attribute 'variable-pitch nil :font "Iosevka SS15")
    )
  )
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "Intel One Mono" (font-family-list))
    ;; (set-face-attribute 'default nil :font "IntelOneMono" :height 120)
    (set-face-attribute 'default nil :font "LigalexMono" :height 130)
    (set-face-attribute 'fixed-pitch nil :font "Iosevka SS15")
    (set-face-attribute 'variable-pitch nil :font "Iosevka SS15")
    )))

(set-fontset-font
 t
 (if (version< emacs-version "28.1")
     '(#x1f300 . #x1fad0)
   'emoji)
 (cond
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
  ((member "Symbola" (font-family-list)) "Symbola")
  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
  ))

;; set Chinese font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset
   (font-spec :family
              (cond
               ((eq system-type 'darwin)
                (cond
                 ((member "Sarasa Term SC Nerd" (font-family-list)) "Sarasa Term SC Nerd")
                 ((member "PingFang SC" (font-family-list)) "PingFang SC")
                 ((member "WenQuanYi Zen Hei" (font-family-list)) "WenQuanYi Zen Hei")
                 ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
                 ))
               ((eq system-type 'gnu/linux)
                (cond
                 ((member "Sarasa Term SC Nerd" (font-family-list)) "Sarasa Term SC Nerd")
                 ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei")
                 ((member "WenQuanYi Zen Hei" (font-family-list)) "WenQuanYi Zen Hei")
                 ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
                 ))
               (t
                (cond
                 ((member "Sarasa Term SC Nerd" (font-family-list)) "Sarasa Term SC Nerd")
                 ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
                 )))
              )))

;; set Chinese font scale
(setq face-font-rescale-alist `(
                                ("Symbola"             . 1.3)
                                ("Microsoft YaHei"     . 1.2)
                                ("WenQuanYi Zen Hei"   . 1.2)
                                ("Sarasa Term SC Nerd" . 1.2)
                                ("PingFang SC"         . 1.16)
                                ("Lantinghei SC"       . 1.16)
                                ("Kaiti SC"            . 1.16)
                                ("Yuanti SC"           . 1.16)
                                ("Apple Color Emoji"   . 0.91)
                                ))
(provide 're-font)
