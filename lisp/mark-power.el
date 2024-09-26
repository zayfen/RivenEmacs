;;; -*- coding: utf-8; lexical-binding: t -*-

;; combine with lsp-bridge for better jump

(defconst MARK-POWER--HISTORY-MAX-SIZE 20)

(setq mark-power--mark-enum '(("default" . 0)
		              ("find-def" . 1)
		              ("find-impl" . 2)
		              ("find-typedef" . 3)
		              ("find-peek" . 4)
		              ))

(setq mark-power--mark-to-jump-back-handler '(("default" . xref-go-back)
			                  ("find-def" . lsp-bridge-find-def-return)
			                  ("find-impl" . xref-go-back)
                        ("find-typedef" . xref-go-back)
			                  ("find-peek" . lsp-bridge-peek-jump-back)))

(defun get-mark-value (mark-key)
  "return value of enum whoes key is {mark-key}"
  (cdr (assoc mark-key mark-power--mark-enum))
  )

(defun get-mark-key (mark-value)
  "return key of enum whoes value is {mark-value}"
  (let ((mark-key (car (rassoc mark-value mark-power--mark-enum))))
    (if mark-key mark-key
      "default")
    ))

(defun call-mark-jump-back-handler (mark-key)
  "get handler map to {mark}, and call it"
  (let ((fn (assoc-default mark-key mark-power--mark-to-jump-back-handler)))
    (funcall fn)))

(setq mark-history '())

(defun mark-power--set-mark (type)
  "push a mark to mark-history"
  (let ((mark-value (get-mark-value type)))
    (if (>= (length mark-history) MARK-POWER--HISTORY-MAX-SIZE)
        (progn
	        (pop mark-history)
	        (setq mark-history (cons mark-value mark-history)))

      (setq mark-history (cons mark-value mark-history)))))

(defun mark-power--jump-back ()
  "jump back to last position"
  (interactive)
  (let ((mark-value (pop mark-history)))
    (call-mark-jump-back-handler (get-mark-key mark-value))))


(provide 'mark-power)
