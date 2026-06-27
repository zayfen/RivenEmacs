;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'ert)
(require 'emacs-codeforces-scrape)

(ert-deftest +cf--problem-url/format ()
  "Problem URL is built from contestId and index."
  (should (equal (+cf--problem-url '(:contestId 1234 :index "A"))
                 "https://codeforces.com/problemset/problem/1234/A"))
  (should (equal (+cf--problem-url '(:contestId 1234 :index "F2"))
                 "https://codeforces.com/problemset/problem/1234/F2")))

(ert-deftest +cf--submit-url/format ()
  "Submit URL targets the contest submit page."
  (should (equal (+cf--submit-url '(:contestId 1234 :index "A"))
                 "https://codeforces.com/contest/1234/submit")))

(ert-deftest +cf--detect-cloudflare/flags-challenge ()
  "Cloudflare challenge pages are detected."
  (should (+cf--detect-cloudflare "Please wait... Just a moment... cf-chl"))
  (should-not (+cf--detect-cloudflare "<html>normal page</html>")))
