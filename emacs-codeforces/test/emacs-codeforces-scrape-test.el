;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'ert)
(require 'emacs-codeforces-scrape)

(defconst +cf-test/statement-html
  "<html><body>
<div class=\"problem-statement\">
  <div class=\"title\">A. Two Rabbits</div>
  <div class=\"time-limit\">2 seconds</div>
  <div class=\"memory-limit\">256 megabytes</div>
  <div class=\"input-specification\"><div class=\"section-title\">Input</div><p>The first line.</p></div>
  <div class=\"input\"><div class=\"section-title\">Input</div><pre>1 2 3</pre></div>
  <div class=\"output\"><div class=\"section-title\">Output</div><pre>1</pre></div>
</div>
</body></html>")

(ert-deftest +cf--extract-problem-statement/locates-div ()
  "The <div class=problem-statement> is extracted from the page."
  (let ((stmt (+cf--extract-problem-statement +cf-test/statement-html)))
    (should (string-match-p "problem-statement" stmt))
    (should (string-match-p "Two Rabbits" stmt))))

(ert-deftest +cf--html-to-org/converts-paragraphs ()
  "<p> becomes text; the title and limits are preserved."
  (let ((org (+cf--html-to-org (+cf--extract-problem-statement +cf-test/statement-html))))
    (should (string-match-p "Two Rabbits" org))
    (should (string-match-p "2 seconds" org))
    (should (string-match-p "The first line." org))))

(ert-deftest +cf--html-to-org/sample-io-as-src-block ()
  "<pre> sample I/O becomes Org src blocks."
  (let ((org (+cf--html-to-org (+cf--extract-problem-statement +cf-test/statement-html))))
    (should (string-match-p "#\\+begin_src fundamental" org))
    (should (string-match-p "1 2 3" org))))

(ert-deftest +cf--detect-cloudflare/flags-challenge ()
  "Cloudflare challenge pages are detected."
  (should (+cf--detect-cloudflare "Please wait... Just a moment... cf-chl"))
  (should-not (+cf--detect-cloudflare "<html>normal page</html>")))
