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
  "URL-encode ALIST as key=value pairs joined by &."
  (mapconcat
   (lambda (cell)
     (concat (url-hexify-string (symbol-name (car cell)))
             "="
             (url-hexify-string (or (cdr cell) ""))))
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

(cl-defun +cf-http-get (url &key params cookie headers timeout)
  "Synchronously GET URL with query PARAMS.
Returns (HTTP-STATUS . BODY-STRING).  COOKIE, when non-nil, is sent as the
Cookie header.  HEADERS is an alist of extra headers.  TIMEOUT seconds."
  (let* ((full-url (+cf--merge-query url params))
         (url-request-extra-headers
          (append (when cookie (list (cons "Cookie" cookie)))
                  headers))
         (url-request-method "GET")
         (url-privacy-level nil)
         (status nil)
         (body nil))
    (condition-case err
        (with-current-buffer
            (let ((url-http-attempt-keepalive nil))
              (url-retrieve-synchronously full-url nil nil (or timeout +cf-default-timeout)))
          (goto-char (point-min))
          (if (not (search-forward "\n\n" nil t))
              (progn
                (setq status 0 body "Empty HTTP response."))
            (setq status (or (and (boundp 'url-http-response-status)
                                  url-http-response-status)
                             200))
            (setq body (buffer-substring-no-properties (point) (point-max)))))
      (error
       (setq status 0 body (error-message-string err))))
    (cons status body)))

(cl-defun +cf-http-post (url data-alist &key cookie headers timeout)
  "Synchronously POST to URL with form DATA-ALIST (x-www-form-urlencoded).
COOKIE, HEADERS, TIMEOUT as in `+cf-http-get'.  Returns (STATUS . BODY)."
  (let* ((url-request-method "POST")
         (url-request-data (+cf--url-encode-alist data-alist))
         (url-request-extra-headers
          (append (list (cons "Content-Type"
                              "application/x-www-form-urlencoded"))
                  (when cookie (list (cons "Cookie" cookie)))
                  headers))
         (status nil)
         (body nil))
    (condition-case err
        (with-current-buffer
            (url-retrieve-synchronously url nil nil (or timeout +cf-default-timeout))
          (goto-char (point-min))
          (if (not (search-forward "\n\n" nil t))
              (setq status 0 body "Empty HTTP response.")
            (setq status (or (and (boundp 'url-http-response-status)
                                  url-http-response-status)
                             200))
            (setq body (buffer-substring-no-properties (point) (point-max)))))
      (error
       (setq status 0 body (error-message-string err))))
    (cons status body)))

(defun +cf-api-get (method &rest params)
  "Call public JSON API METHOD with PARAMS (alternating :key value).
Returns the parsed `result' plist on success, or signals an error with the
API status message on failure."
  (let* ((alist (cl-loop for (k v) on params by #'cddr
                         collect (cons (substring (symbol-name k) 1) v)))
         (url (+cf--merge-query (concat +cf-api-base method) alist))
         (resp (+cf-http-get url))
         (status (car resp))
         (json (+cf--parse-json-body (cdr resp))))
    (if (and (= status 200)
             (equal (plist-get json :status) "OK"))
        (plist-get json :result)
      (error "Codeforces API %s failed: %s" method
             (or (plist-get json :comment) (cdr resp))))))

(provide 'emacs-codeforces-client)

;;; emacs-codeforces-client.el ends here
