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

;;; Response buffer lifecycle and readable API errors

(require 'cl-lib)

(defun +cf-client-test--fake-response-buffer (content)
  "Return a fresh buffer containing the raw url response CONTENT."
  (let ((buffer (generate-new-buffer " *cf-client-test-http*")))
    (with-current-buffer buffer
      (insert content))
    buffer))

(ert-deftest +cf-http-get/kills-response-buffer ()
  "GET returns status/body and kills the url response buffer."
  (let ((buffer (+cf-client-test--fake-response-buffer "HTTP/1.1 200 OK\n\nBODY")))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) buffer)))
      (should (equal (+cf-http-get "https://codeforces.test/api/x")
                     (cons 200 "BODY"))))
    (should-not (buffer-live-p buffer))))

(ert-deftest +cf-http-post/kills-response-buffer ()
  "POST returns status/body and kills the url response buffer."
  (let ((buffer (+cf-client-test--fake-response-buffer "HTTP/1.1 200 OK\n\nCREATED")))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) buffer)))
      (should (equal (+cf-http-post "https://codeforces.test/api/x" '((a . "1")))
                     (cons 200 "CREATED"))))
    (should-not (buffer-live-p buffer))))

(ert-deftest +cf-api-get/reports-http-failure-before-json-parsing ()
  "An HTTP failure is reported with its status and body, not a JSON error."
  (cl-letf (((symbol-function '+cf-http-get)
             (lambda (&rest _) (cons 500 "<html>boom</html>"))))
    (let ((error-data (should-error (+cf-api-get "user.status" :handle "x")
                                    :type 'error)))
      (should (string-match-p "HTTP 500" (error-message-string error-data)))
      (should (string-match-p "boom" (error-message-string error-data))))))

(ert-deftest +cf-api-get/reports-invalid-json ()
  "A non-JSON 200 response produces a readable invalid-JSON error."
  (cl-letf (((symbol-function '+cf-http-get)
             (lambda (&rest _) (cons 200 "<html>not json</html>"))))
    (let ((error-data (should-error (+cf-api-get "user.status" :handle "x")
                                    :type 'error)))
      (should (string-match-p "invalid JSON" (error-message-string error-data))))))

(ert-deftest +cf-api-get/reports-api-comment ()
  "A FAILED JSON payload is reported with its API comment."
  (cl-letf (((symbol-function '+cf-http-get)
             (lambda (&rest _)
               (cons 200 "{\"status\":\"FAILED\",\"comment\":\"nope\"}"))))
    (let ((error-data (should-error (+cf-api-get "user.status" :handle "x")
                                    :type 'error)))
      (should (string-match-p "nope" (error-message-string error-data))))))
