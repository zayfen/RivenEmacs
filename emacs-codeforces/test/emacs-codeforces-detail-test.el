;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'ert)
(require 'emacs-codeforces-detail)

(ert-deftest +cf--format-verdict/queued ()
  (should (equal (+cf--format-verdict '(:id 1))
                 "Queued / compiling…")))

(ert-deftest +cf--format-verdict/testing ()
  (should (equal (+cf--format-verdict '(:id 1 :verdict "TESTING" :passedTestCount 12))
                 "Running… passed 12 tests.")))

(ert-deftest +cf--format-verdict/ok ()
  (should (string-match-p "OK"
                          (+cf--format-verdict
                           '(:id 1 :verdict "OK" :passedTestCount 30
                                  :timeConsumedMillis 124 :memoryConsumedBytes 8388608)))))

(ert-deftest +cf--format-verdict/wa ()
  (should (string-match-p "WRONG_ANSWER"
                          (+cf--format-verdict
                           '(:id 1 :verdict "WRONG_ANSWER" :passedTestCount 13)))))

(ert-deftest +cf--terminal-verdict-p/classifies ()
  (should (+cf--terminal-verdict-p "OK"))
  (should (+cf--terminal-verdict-p "WRONG_ANSWER"))
  (should (+cf--terminal-verdict-p 'OK))            ; symbol tolerance
  (should-not (+cf--terminal-verdict-p "TESTING"))
  (should-not (+cf--terminal-verdict-p nil)))
