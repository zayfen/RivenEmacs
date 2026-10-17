;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-client.el --- HTTP client for Codeforces -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin synchronous wrapper around `url.el' for Codeforces requests.
;; Public read-only JSON calls go to `+cf-api-base'.  Website (cookie) calls
;; go to `+cf-site-base'.  Returns a cons (HTTP-STATUS-CODE . BODY-STRING) for
;; raw fetches, or a plist for JSON fetches.
;;
;; Cookie *content* lives in `emacs-codeforces-auth'; this module only knows
;; how to attach a cookie header string when one is supplied.

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'json)

(defconst +cf-api-base "https://codeforces.com/api/"
  "Base URL for the public Codeforces JSON API.")

(defconst +cf-site-base "https://codeforces.com/"
  "Base URL for the Codeforces website (login, submit, statements).")

(defconst +cf-default-timeout 30
  "Default HTTP timeout in seconds.")

(defun +cf--url-encode-alist (alist)
  "URL-encode ALIST as key=value pairs joined by &.
Keys may be symbols or strings."
  (mapconcat
   (lambda (cell)
     (let ((key (car cell))
           (val (cdr cell)))
       (concat (url-hexify-string
                (cond ((symbolp key) (symbol-name key))
                      ((numberp key) (number-to-string key))
                      (t key)))
               "="
               (url-hexify-string (if val
                                      (if (numberp val)
                                          (number-to-string val)
                                        val)
                                    "")))))
   alist "&"))

(defun +cf--merge-query (url params)
  "Append PARAMS (alist) to URL as a query string.
Preserves an existing query string in URL."
  (if (seq-empty-p params)
      url
    (let ((sep (if (string-match-p "\\?" url) "&" "?")))
      (concat url sep (+cf--url-encode-alist params)))))

(defun +cf--parse-json-body (body)
  "Parse JSON string BODY into a plist.
`json-array-type' is list, `json-object-type' is plist."
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-false nil))
    (json-read-from-string body)))

(defun +cf--body-summary (body)
  "Return a short single-line summary of HTTP BODY for error messages."
  (let ((text (string-trim (or body ""))))
    (if (> (length text) 200)
        (concat (substring text 0 200) "...")
      text)))

(defun +cf--http-response (buffer)
  "Return (HTTP-STATUS . BODY-STRING) read from url response BUFFER.
BUFFER is killed by the caller; a nil or dead BUFFER yields status 0."
  (if (not (buffer-live-p buffer))
      (cons 0 "Empty HTTP response.")
    (with-current-buffer buffer
      (goto-char (point-min))
      (if (not (search-forward "\n\n" nil t))
          (cons 0 "Empty HTTP response.")
        (cons (or (and (boundp 'url-http-response-status)
                       url-http-response-status)
                  200)
              (buffer-substring-no-properties (point) (point-max)))))))

(cl-defun +cf-http-get (url &key params cookie headers timeout)
  "Synchronously GET URL with query PARAMS.
Returns (HTTP-STATUS . BODY-STRING).  COOKIE, when non-nil, is sent as the
Cookie header.  HEADERS is an alist of extra headers.  TIMEOUT seconds.
The response buffer is always killed before returning."
  (let* ((full-url (+cf--merge-query url params))
         (url-request-extra-headers
          (append (when cookie (list (cons "Cookie" cookie)))
                  headers))
         (url-request-method "GET")
         (url-privacy-level nil)
         (resp-buffer nil)
         (result nil))
    (unwind-protect
        (setq result
              (condition-case err
                  (progn
                    (setq resp-buffer
                          (let ((url-http-attempt-keepalive nil))
                            (url-retrieve-synchronously
                             full-url nil nil (or timeout +cf-default-timeout))))
                    (+cf--http-response resp-buffer))
                (error
                 (cons 0 (error-message-string err)))))
      (when (buffer-live-p resp-buffer)
        (kill-buffer resp-buffer)))
    result))

(cl-defun +cf-http-post (url data-alist &key cookie headers timeout)
  "Synchronously POST to URL with form DATA-ALIST (x-www-form-urlencoded).
COOKIE, HEADERS, TIMEOUT as in `+cf-http-get'.  Returns (STATUS . BODY).
The response buffer is always killed before returning."
  (let* ((url-request-method "POST")
         (url-request-data (+cf--url-encode-alist data-alist))
         (url-request-extra-headers
          (append (list (cons "Content-Type"
                              "application/x-www-form-urlencoded"))
                  (when cookie (list (cons "Cookie" cookie)))
                  headers))
         (resp-buffer nil)
         (result nil))
    (unwind-protect
        (setq result
              (condition-case err
                  (progn
                    (setq resp-buffer
                          (url-retrieve-synchronously
                           url nil nil (or timeout +cf-default-timeout)))
                    (+cf--http-response resp-buffer))
                (error
                 (cons 0 (error-message-string err)))))
      (when (buffer-live-p resp-buffer)
        (kill-buffer resp-buffer)))
    result))

(defun +cf-api-get (method &rest params)
  "Call public JSON API METHOD with PARAMS (alternating :key value).
Returns the parsed `result' on success.  Signals an error carrying the
HTTP status and a short body summary when the request fails, and the API
comment when the JSON payload reports a failure."
  (let* ((alist (cl-loop for (k v) on params by #'cddr
                         collect (cons (substring (symbol-name k) 1) v)))
         (url (+cf--merge-query (concat +cf-api-base method) alist))
         (resp (+cf-http-get url))
         (status (car resp))
         (body (cdr resp)))
    (unless (= status 200)
      (error "Codeforces API %s failed (HTTP %s): %s"
             method status (+cf--body-summary body)))
    (let ((json (condition-case err
                    (+cf--parse-json-body body)
                  (error
                   (error "Codeforces API %s returned invalid JSON: %s"
                          method (error-message-string err))))))
      (if (equal (plist-get json :status) "OK")
          (plist-get json :result)
        (error "Codeforces API %s failed: %s" method
               (or (plist-get json :comment)
                   (+cf--body-summary body)))))))

(provide 'emacs-codeforces-client)

;;; emacs-codeforces-client.el ends here
