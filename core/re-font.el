;;; re-font.el --- font settings -*- lexical-binding: t; -*-

;; set default font
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas" t t)))
 ((string-equal system-type "darwin") ; macOS
  (when (member "Menlo" (font-family-list))
    (set-frame-font "Menlo" t t)))
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-frame-font "Ligalex Mono-13" t t))))


;; set font for symbols
(set-fontset-font
 t
 'symbol
 (cond
  ((string-equal system-type "windows-nt")
   (cond
    ((member "Segoe UI Symbol" (font-family-list)) "Segoe UI Symbol")))
  ((string-equal system-type "darwin")
   (cond
    ((member "Apple Symbols" (font-family-list)) "Apple Symbols")))
  ((string-equal system-type "gnu/linux")
   (cond
    ((member "Symbola" (font-family-list)) "Symbola")))))

;; set font for chinese
;;(dolist (charset '(kana han symbol cjk-misc bopomofo))
(defun +set-chinese-font()
  (interactive)
  (set-fontset-font
   t
   'han
   (cond
    ((string-equal system-type "windows-nt")
     (cond
      ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
      ((member "Microsoft JhengHei" (font-family-list)) "Microsoft JhengHei")
      ((member "SimHei" (font-family-list)) "SimHei")))
    ((string-equal system-type "darwin")
     (cond
      ((member "Hei" (font-family-list)) "Hei")
      ((member "Heiti SC" (font-family-list)) "Heiti SC")
      ((member "Heiti TC" (font-family-list)) "Heiti TC")))
    ((string-equal system-type "gnu/linux")
     (cond
      ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei"))))))

(add-hook 'buffer-list-update-hook '+set-chinese-font)

;; (custom-set-faces '(fixed-pitch ((t (:family "Ligalex Mono" :height 105))))
;;                   '(variable-pitch ((t (:family "Ligalex Mono" :height 140)))))
(provide 're-font)
