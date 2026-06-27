;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-scrape.el --- Website scraping (statements + submit) -*- lexical-binding: t; -*-

;;; Commentary:
;; Two responsibilities that need the cookie-authenticated website:
;;   1. Fetch a problem statement and convert its HTML to Org.
;;   2. Submit a solution (csrf + form POST), with Cloudflare detection and
;;      browser fallback.
;;
;; The HTML->Org converter is pure and unit-tested against fixture HTML.

;;; Code:

(require 'dom)
(require 'cl-lib)
(require 'emacs-codeforces-client)
(require 'emacs-codeforces-auth)

;; ---------- Statement fetch + conversion ----------

(defun +cf--problem-url (problem)
  "Return the website URL for PROBLEM's statement."
  (format "%sproblemset/problem/%s/%s"
          +cf-site-base
          (plist-get problem :contestId)
          (plist-get problem :index)))

(defun +cf--extract-problem-statement (html)
  "Return the inner HTML of the first <div class=\"problem-statement\"> block."
  (if (not (string-match "<div[^>]*class=\"problem-statement\"[^>]*>\\(\\(?:.\\|\n\\)*?\\)</div>\\s-*</div>\\s-*</div>" html))
      html                            ; fallback: return whole page
    (match-string 0 html)))

(defun +cf--html-node-to-org (node)
  "Recursively convert a parsed HTML NODE (dom) to an Org string."
  (cond
   ((stringp node) node)
   ((null node) "")
   (t
    (pcase (dom-tag node)
      ('p (format "%s\n\n" (string-trim (mapconcat #'+cf--html-node-to-org (dom-children node) ""))))
      ('pre (format "#+begin_src fundamental\n%s\n#+end_src\n"
                    (string-trim (mapconcat #'identity (dom-strings node) ""))))
      ('ul (mapconcat
           (lambda (li) (format "- %s" (string-trim (mapconcat #'+cf--html-node-to-org (dom-children li) ""))))
           (dom-by-tag node 'li) "\n"))
      ('ol (mapconcat
           (lambda (li) (format "1. %s" (string-trim (mapconcat #'+cf--html-node-to-org (dom-children li) ""))))
           (dom-by-tag node 'li) "\n"))
      ('br "\n")
      ('b (format "*%s*" (mapconcat #'+cf--html-node-to-org (dom-children node) "")))
      ('i (format "/%s/" (mapconcat #'+cf--html-node-to-org (dom-children node) "")))
      ('span (mapconcat #'+cf--html-node-to-org (dom-children node) ""))
      ('div (mapconcat #'+cf--html-node-to-org (dom-children node) ""))
      ('em (mapconcat #'+cf--html-node-to-org (dom-children node) ""))
      (_ (mapconcat #'+cf--html-node-to-org (dom-children node) ""))))))

(defun +cf--html-to-org (html)
  "Convert statement HTML string to an Org string."
  (with-temp-buffer
    (insert html)
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      ;; take body subtree
      (let ((body (or (dom-by-tag dom 'body) (list dom))))
        (string-trim (mapconcat #'+cf--html-node-to-org body ""))))))

(defun +cf-fetch-problem-html (problem)
  "Fetch the raw statement HTML for PROBLEM (cookie-authenticated)."
  (let ((cookie (+cf--cookie-header)))
    (unless cookie
      (error "Not logged in.  Run M-x codeforces-login"))
    (cdr (+cf-http-get (+cf--problem-url problem) :cookie cookie))))

(defun +cf-fetch-problem-org (problem)
  "Fetch PROBLEM's statement and return Org text."
  (+cf--html-to-org (+cf--extract-problem-statement
                     (+cf-fetch-problem-html problem))))

;; ---------- Submit ----------

(defun +cf--detect-cloudflare (body)
  "Return non-nil if BODY looks like a Cloudflare challenge page."
  (or (string-match-p "Just a moment" (or body ""))
      (string-match-p "cf-chl" (or body ""))
      (string-match-p "cf-browser-verification" (or body ""))))

(defun +cf--submit-url ()
  "Return the problemset submit URL."
  (concat +cf-site-base "problemset/submit"))

(defun +cf--parse-csrf-token (html)
  "Extract the csrf token from the submit page HTML."
  (or (and (string-match "data-csrf=['\"]\\([^'\"]+\\)['\"]" html)
           (match-string 1 html))
      (and (string-match "name=['\"]csrf_token['\"]\\s-*value=['\"]\\([^'\"]+\\)['\"]" html)
           (match-string 1 html))
      (error "Could not find csrf_token on submit page")))

(defun +cf--parse-language-options (html)
  "Return an alist ((id . label) ...) from the submit page's language select."
  (let ((select (if (string-match "<select[^>]*name=['\"]programTypeId['\"][^>]*>\\(\\(?:.\\|\n\\)*?\\)</select>" html)
                    (match-string 1 html)
                  html))
        (options nil))
    ;; iterate over <option value="id">label</option>
    (with-temp-buffer
      (insert select)
      (goto-char (point-min))
      (while (re-search-forward "<option[^>]*value=['\"]\\([0-9]+\\)['\"][^>]*>\\([^<]*\\)</option>" nil t)
        (push (cons (match-string 1) (string-trim (match-string 2))) options)))
    (nreverse options)))

(defun +cf--language-id (options language)
  "Find the programTypeId whose label matches LANGUAGE."
  (let ((needle (pcase language
                  ("rust" "Rust")
                  ("python" "Python")
                  ("cpp" "G++")
                  ("java" "Java")
                  (_ (error "No label match for %s" language)))))
    (or (car (cl-find-if (lambda (cell)
                           (string-match-p needle (cdr cell)))
                         options))
        (error "Language %s not found on submit page" language))))

(cl-defun +cf-submit-via-http (problem source language)
  "Submit SOURCE as LANGUAGE for PROBLEM via website form POST.
Returns the submission id (string) on success, or the symbol
`cloudflare' if a challenge page was detected (caller should fall back
to `+cf-submit-via-browser')."
  (let ((cookie (+cf--cookie-header)))
    (unless cookie
      (error "Not logged in.  Run M-x codeforces-login"))
    (let* ((page-resp (+cf-http-get (+cf--submit-url) :cookie cookie))
           (page-body (cdr page-resp)))
      (when (+cf--detect-cloudflare page-body)
        (cl-return-from +cf-submit-via-http 'cloudflare))
      (let* ((csrf (+cf--parse-csrf-token page-body))
             (lang-id (+cf--language-id
                       (+cf--parse-language-options page-body)
                       language))
             (resp (+cf-http-post
                    (+cf--submit-url)
                    `((csrf_token . ,csrf)
                      (action . "submitSolutionFormSubmitted")
                      (submittedProblemIndex . ,(plist-get problem :index))
                      (contestId . ,(number-to-string (plist-get problem :contestId)))
                      (programTypeId . ,lang-id)
                      (source . ,source)
                      (tabSize . "4"))
                    :cookie cookie))
             (resp-body (cdr resp)))
        (cond
         ((+cf--detect-cloudflare resp-body) 'cloudflare)
         ;; success redirects to /submissions/<handle>/<id>
         ((string-match "/submissions/\\([^/]+\\)/\\([0-9]+\\)" resp-body)
          (match-string 2 resp-body))
         (t (error "Submit did not return a submission id.  Check credentials.")))))))

(defun +cf-submit-via-browser (problem _language)
  "Open the pre-filled submit page for PROBLEM in a browser.
Used as the Cloudflare fallback."
  (let ((url (format "%scontest/%s/submit"
                     +cf-site-base
                     (plist-get problem :contestId))))
    (browse-url url)
    (message "Cloudflare blocked direct submit.  Submit '%s' in the browser; results tracked here."
             (plist-get problem :index))))

(provide 'emacs-codeforces-scrape)

;;; emacs-codeforces-scrape.el ends here
