;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-poll-test.el --- Verdict poll timer tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline tests for the submission verdict poller: the timer must update the
;; detail buffer it was started from even when it fires while another buffer is
;; current, and it must cancel itself when that detail buffer is killed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-codeforces-detail)

(defun +cf-poll-test--fire-timer (timer)
  "Run TIMER's callback now, from whatever buffer is current."
  (apply (timer--function timer) (timer--args timer)))

(defun +cf-poll-test--detail-buffer ()
  "Return a detail-like buffer with the submission status section."
  (let ((buffer (generate-new-buffer "*CF 1_A poll-test*")))
    (with-current-buffer buffer
      (insert "** Submission Status\n*No active submission.*\n"))
    buffer))

(ert-deftest +cf--terminal-verdict-p/includes-all-terminal-values ()
  "Every terminal Codeforces verdict is recognized; TESTING/nil are not."
  (dolist (verdict '("OK" "WRONG_ANSWER" "TIME_LIMIT_EXCEEDED"
                     "MEMORY_LIMIT_EXCEEDED" "RUNTIME_ERROR"
                     "COMPILATION_ERROR" "PARTIAL" "PRESENTATION_ERROR"
                     "IDLENESS_LIMIT_EXCEEDED" "SECURITY_VIOLATED" "CRASHED"
                     "INPUT_PREPARATION_CRASHED" "CHALLENGED" "SKIPPED"
                     "REJECTED" "FAILED"))
    (should (+cf--terminal-verdict-p verdict)))
  (should-not (+cf--terminal-verdict-p "TESTING"))
  (should-not (+cf--terminal-verdict-p nil)))

(ert-deftest +cf--start-poll/updates-captured-buffer ()
  "A timer firing from another buffer still updates the detail buffer."
  (let ((detail (+cf-poll-test--detail-buffer))
        (other (generate-new-buffer " *cf-poll-test-other*")))
    (unwind-protect
        (cl-letf (((symbol-function '+cf-fetch-submission)
                   (lambda (&rest _)
                     '(:id 42 :verdict "OK" :passedTestCount 3))))
          (with-current-buffer detail
            (+cf--start-poll 1 42))
          (let ((timer (buffer-local-value '+cf--poll-timer detail)))
            (should (timerp timer))
            (unwind-protect
                (progn
                  ;; Fire the timer while an unrelated buffer is current.
                  (with-current-buffer other
                    (+cf-poll-test--fire-timer timer))
                  (with-current-buffer detail
                    (should (string-match-p "OK" (buffer-string)))
                    (should-not +cf--poll-timer))
                  (with-current-buffer other
                    (should-not (string-match-p "Submission Status"
                                                (buffer-string)))))
              (when (timerp timer)
                (cancel-timer timer)))))
      (when (buffer-live-p detail) (kill-buffer detail))
      (when (buffer-live-p other) (kill-buffer other)))))

(ert-deftest +cf--start-poll/dead-buffer-cancels-timer ()
  "A timer whose detail buffer was killed cancels itself without error."
  (let ((detail (+cf-poll-test--detail-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer detail
            (+cf--start-poll 1 43))
          (let ((timer (buffer-local-value '+cf--poll-timer detail)))
            (should (timerp timer))
            (unwind-protect
                (progn
                  (kill-buffer detail)
                  ;; Firing from the (now unrelated) current buffer must not
                  ;; signal and must remove the timer from the timer list.
                  (with-temp-buffer
                    (+cf-poll-test--fire-timer timer))
                  (should-not (memq timer timer-list)))
              (when (timerp timer)
                (cancel-timer timer)))))
      (when (buffer-live-p detail) (kill-buffer detail)))))

(provide 'emacs-codeforces-poll-test)
;;; emacs-codeforces-poll-test.el ends here
