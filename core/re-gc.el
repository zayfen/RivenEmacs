;; re-gc.el --- RivenEmacs -*- lexical-binding: t; -*-



(use-package gcmh
  :straight t
  :hook (rivenemacs-lazy . gcmh-mode)
  :custom
  ;; Set the delay to 20s instead of the default 15. I tried using `auto', but
  ;; with the default 20 of `gcmh-auto-idle-delay-factor', it triggers GC each
  ;; 1s on my machine. Setting the factor to a higher value should solve the
  ;; issue on my machine, but I don't think it is right to assume it will work
  ;; the same way on other machines. So we switch back to a fixed delay of 20s.
  (gcmh-idle-delay 30)
  ;; The default `gcmh's 1GB is probably too high. We set it to 256MB on 64bit
  ;; systems, or 16MB on 32bit ones.
  (gcmh-high-cons-threshold
   (* 32 1024 (if (string-suffix-p "64" (symbol-name sys/arch)) 1024 512))))


(provide 're-gc)
