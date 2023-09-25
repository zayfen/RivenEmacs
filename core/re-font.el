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
    ;;(set-frame-font "Ligalex Mono 12" t t)
    ;; (set-frame-font "IntelOneMono 12" t t)
    (set-frame-font "Iosevka SS15 14" t t)
    ;; (set-frame-font "CascadiaMono 12" t t)
    ;; (set-frame-font "FiraCode 12" t t)
    )))

;; set font for symbols
;; (set-fontset-font
;;  t
;;  'symbol
;;  (cond
;;   ((string-equal system-type "windows-nt")
;;    (cond
;;     ((member "Segoe UI Symbol" (font-family-list)) "Segoe UI Symbol")))
;;   ((string-equal system-type "darwin")
;;    (cond
;;     ((member "Apple Symbols" (font-family-list)) "Apple Symbols")))
;;   ((string-equal system-type "gnu/linux")
;;    (cond
;;     ((member "Symbola" (font-family-list)) "Symbola")))))

(provide 're-font)
