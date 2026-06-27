;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'ert)
(require 'emacs-codeforces-client)

(ert-deftest +cf--url-encode-alist/encodes-pairs ()
  "Alist pairs become key=value joined by &, URL-encoded."
  (should
   (equal (+cf--url-encode-alist '((contestId . "566")
                                   (tags . "dp,greedy")))
          "contestId=566&tags=dp%2Cgreedy")))

(ert-deftest +cf--merge-query/appends-params ()
  "Base URL with no existing query gets ?params; existing query gets &params."
  (should (equal (+cf--merge-query "https://x/api/m" '((a . "1")))
                 "https://x/api/m?a=1"))
  (should (equal (+cf--merge-query "https://x/api/m?z=9" '((a . "1")))
                 "https://x/api/m?z=9&a=1")))

(ert-deftest +cf--parse-json-body/returns-plist ()
  "JSON object string is parsed into a plist."
  (should (equal (+cf--parse-json-body "{\"status\":\"OK\",\"x\":3}")
                 '(:status "OK" :x 3))))
