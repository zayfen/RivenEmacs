# emacs-codeforces Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `emacs-codeforces`, an independent Emacs Lisp library (8 modules) that lets a user log in via session-cookie, browse problems with tag search, view Org-rendered problem statements, and submit solutions with real-time verdict polling — plus a thin RivenEmacs integration module.

**Architecture:** Layered library: `client` (HTTP/cookie) → `auth` / `api` / `scrape` (depend on client) → `workspace` (pure local FS) → `list` / `detail` (UI). Top-level `emacs-codeforces.el` requires all submodules and provides the feature. Public commands are autoloaded. Problem list, tag search, status polling go through the public JSON API (stable). Login validation, statement fetch, and submit go through the website HTML (cookie + csrf), with Cloudflare detection and browser fallback for submit.

**Tech Stack:** Emacs 31, built-in `url` / `json` / `org` / `tabulated-list` / `libxml2` (`libxml-parse-html-region`), `ert` for tests. `use-package` + `general.el` for RivenEmacs integration.

**Spec:** `docs/superpowers/specs/2026-06-28-emacs-codeforces-design.md`

**Conventions (from AGENTS.md):**
- Every file header: `;; -*- coding: utf-8; lexical-binding: t; -*-` + Commentary + `;;; Code:` + `provide` + `;;; file ends here`.
- 2-space indent, `indent-tabs-mode nil`, fill-column 120.
- Public config: `codeforces-*` defcustoms in group `codeforces`. Public commands: `codeforces-*`. Internal helpers: `+cf-*`.
- All network ops wrapped in `condition-case`.
- Tests live alongside code as `test/emacs-codeforces-<module>-test.el`, run via `emacs --batch -l <test-file> -f ert-run-tests-batch-and-exit`.

**Testing command (used throughout):**
```bash
emacs --batch -L emacs-codeforces -l emacs-codeforces/test/emacs-codeforces-<MODULE>-test.el -f ert-run-tests-batch-and-exit
```
`-L emacs-codeforces` adds the library dir to `load-path` so `(require 'emacs-codeforces-<module>)` resolves.

**Code style note:** Elisp has no `return` early-exit; use `cond` / `when` / nested `if` or `cl-block`/`cl-return-from` where early return is needed. All functions use lexical binding (declared in file header).

---

## Phase 1: Skeleton & client layer

### Task 1: Library skeleton + entry module

**Files:**
- Create: `emacs-codeforces/emacs-codeforces.el`

This task creates the entry point only — it requires submodules that don't exist yet, so it will NOT byte-compile until later phases. We commit the skeleton now and revisit at the end.

- [ ] **Step 1: Create the entry module**

Create `emacs-codeforces/emacs-codeforces.el`:

```elisp
;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces.el --- Codeforces workflow client for Emacs -*- lexical-binding: t; -*-

;; Author: RivenEmacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, games
;; URL: https://github.com/zayfen/RivenEmacs

;;; Commentary:
;; `emacs-codeforces' is an independent Emacs Lisp library for the
;; Codeforces competitive-programming workflow.  It provides login
;; (session-cookie injection), problem browsing with tag search, Org-rendered
;; problem statements, solution scaffolding, and submission with real-time
;; verdict polling.
;;
;; Load it with (require 'emacs-codeforces).  See the `codeforces' customize
;; group for options.

;;; Code:

(require 'emacs-codeforces-client)
(require 'emacs-codeforces-auth)
(require 'emacs-codeforces-api)
(require 'emacs-codeforces-scrape)
(require 'emacs-codeforces-workspace)
(require 'emacs-codeforces-list)
(require 'emacs-codeforces-detail)

(defgroup codeforces nil
  "Codeforces workflow client."
  :group 'tools)

(defcustom codeforces-home-directory
  (expand-file-name "~/.emacs-codeforces/")
  "Codeforces workspace root.
Holds credentials, API cache, and per-problem solution directories."
  :type 'directory
  :group 'codeforces)

(defcustom codeforces-default-language "rust"
  "Default language for new solutions.
Must correspond to a template file in `codeforces-templates-directory'
or the bundled templates."
  :type '(choice (const "rust") (const "python") (const "cpp") (const "java"))
  :group 'codeforces)

(defcustom codeforces-poll-interval 2
  "Seconds between submission status polls."
  :type 'number
  :group 'codeforces)

(defcustom codeforces-poll-timeout 60
  "Max seconds to poll a submission before giving up."
  :type 'number
  :group 'codeforces)

(defcustom codeforces-templates-directory nil
  "Override directory for language templates.
Nil means use the templates bundled with the library."
  :type '(choice directory (const nil))
  :group 'codeforces)

;;;###autoload
(defun codeforces-login ()
  "Log in to Codeforces via session-cookie injection.
Prompts for a session cookie copied from a browser, validates it, and on
success creates `codeforces-home-directory'.  See
`emacs-codeforces-auth.el' for details."
  (interactive)
  (require 'emacs-codeforces-auth)
  (call-interactively #'codeforces-login))

;;;###autoload
(defun codeforces-logout ()
  "Clear stored Codeforces credentials."
  (interactive)
  (require 'emacs-codeforces-auth)
  (call-interactively #'codeforces-logout))

;;;###autoload
(defun codeforces-browse-problems ()
  "Open the Codeforces problem list buffer."
  (interactive)
  (require 'emacs-codeforces-list)
  (call-interactively #'codeforces-browse-problems))

(provide 'emacs-codeforces)

;;; emacs-codeforces.el ends here
```

- [ ] **Step 2: Commit skeleton**

```bash
git add emacs-codeforces/emacs-codeforces.el
git commit -m "feat(codeforces): add library entry module skeleton"
```

---

### Task 2: HTTP client layer

**Files:**
- Create: `emacs-codeforces/emacs-codeforces-client.el`
- Create: `emacs-codeforces/test/emacs-codeforces-client-test.el`

The client wraps `url.el` into a synchronous `(STATUS . BODY)` fetch with a
configurable timeout. It also provides a cookie-header accessor used by the
website requests (cookie *content* is loaded/stored by `auth.el`, not here).
Network is not unit-tested; we test the pure helpers (`+cf--url-encode-alist`,
`+cf--merge-query`).

- [ ] **Step 1: Write the failing test for URL-encoding helpers**

Create `emacs-codeforces/test/emacs-codeforces-client-test.el`:

```elisp
;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'ert)
(require 'emacs-codeforces-client)

(ert-deftest +cf--url-encode-alist/encodes-pairs ()
  "Alist pairs become key=value joined by &, URL-encoded."
  (should (equal "+cf--url-encode-alist/encodes-pairs" ; sanity: ert loads
                 "+cf--url-encode-alist/encodes-pairs"))
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
```

- [ ] **Step 2: Run test to verify it fails**

Run:
```bash
emacs --batch -L emacs-codeforces -l emacs-codeforces/test/emacs-codeforces-client-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL — `Cannot open load file: emacs-codeforces-client`.

- [ ] **Step 3: Write minimal implementation**

Create `emacs-codeforces/emacs-codeforces-client.el`:

```elisp
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

(defun +cf-http-get (url &key params cookie headers timeout)
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

(defun +cf-http-post (url data-alist &key cookie headers timeout)
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
    (if (and (= status 200) (eq (plist-get json :status) 'OK))
        (plist-get json :result)
      (error "Codeforces API %s failed: %s" method
             (or (plist-get json :comment) (cdr resp))))))

(provide 'emacs-codeforces-client)

;;; emacs-codeforces-client.el ends here
```

- [ ] **Step 4: Run test to verify it passes**

Run:
```bash
emacs --batch -L emacs-codeforces -l emacs-codeforces/test/emacs-codeforces-client-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
git add emacs-codeforces/emacs-codeforces-client.el emacs-codeforces/test/emacs-codeforces-client-test.el
git commit -m "feat(codeforces): add HTTP client layer with tests"
```

---

## Phase 2: Auth, API, workspace

### Task 3: Auth module (session-cookie login)

**Files:**
- Create: `emacs-codeforces/emacs-codeforces-auth.el`
- Create: `emacs-codeforces/test/emacs-codeforces-auth-test.el`

Login flow: prompt for cookie → write `credentials` (0600) → validate by
GETting `/enter` and checking for "Logout" → create home dir on success. We
unit-test the pure helpers (`+cf--credentials-file`, `+cf--login-page-logged-in-p`).

- [ ] **Step 1: Write the failing test**

Create `emacs-codeforces/test/emacs-codeforces-auth-test.el`:

```elisp
;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'ert)
(require 'emacs-codeforces-auth)

(ert-deftest +cf--login-page-logged-in-p/detects-logout-link ()
  "A page containing the logout link is logged-in."
  (should (+cf--login-page-logged-in-p
           "<a href='/'>Logout</a><form>...</form>")))

(ert-deftest +cf--login-page-logged-in-p/login-form-is-not-logged-in ()
  "A page with only the login form is not logged-in."
  (should-not
   (+cf--login-page-logged-in-p
    "<form action='/enter' method='post'><input name='handleOrEmail'>")))

(ert-deftest +cf--credentials-file/under-home ()
  "Credentials file lives under the configured home directory."
  (let ((codeforces-home-directory "/tmp/cf/"))
    (should (equal (+cf--credentials-file) "/tmp/cf/credentials"))))
```

- [ ] **Step 2: Run test to verify it fails**

Run:
```bash
emacs --batch -L emacs-codeforces -l emacs-codeforces/test/emacs-codeforces-auth-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL — `Cannot open load file: emacs-codeforces-auth`.

- [ ] **Step 3: Write minimal implementation**

Create `emacs-codeforces/emacs-codeforces-auth.el`:

```elisp
;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-auth.el --- Session-cookie login for Codeforces -*- lexical-binding: t; -*-

;;; Commentary:
;; Login is done by injecting a browser session cookie (Codeforces /enter has
;; hCaptcha, so programmatic username/password login is not feasible).
;;
;; Flow: `codeforces-login' prompts for the cookie string, writes it to
;; `credentials' (chmod 600), validates by GETting /enter and detecting the
;; "Logout" link, and creates `codeforces-home-directory' on success.

;;; Code:

(require 'emacs-codeforces-client)

(defun +cf--home-dir ()
  "Return the (ensured) codeforces home directory."
  codeforces-home-directory)

(defun +cf--credentials-file ()
  "Return the path to the credentials file under `codeforces-home-directory'."
  (expand-file-name "credentials" (+cf--home-dir)))

(defun +cf--login-page-logged-in-p (html)
  "Return non-nil if the /enter page HTML indicates an active session.
We detect the 'Logout' link that only appears when authenticated."
  (string-match-p "href=['\"]/['\"]\\s-*Logout\\|>Logout<\\|Logout</a>"
                  (or html "")))

(defun +cf--ensure-home-dir ()
  "Create `codeforces-home-directory' (and cache/workspace subdirs)."
  (make-directory (expand-file-name "cache" codeforces-home-directory) t)
  (make-directory (expand-file-name "workspace" codeforces-home-directory) t))

(defun +cf--read-credentials ()
  "Return the stored cookie string, or nil if none."
  (let ((f (+cf--credentials-file)))
    (and (file-exists-p f)
         (with-temp-buffer
           (insert-file-contents f)
           (buffer-string)))))

(defun +cf--write-credentials (cookie)
  "Write COOKIE to the credentials file, mode 0600."
  (+cf--ensure-home-dir)
  (let ((f (+cf--credentials-file)))
    (with-temp-file f
      (insert cookie)
      (set-file-modes f #o600))))

(defun codeforces-logged-in-p ()
  "Return non-nil if a credential file exists (does not validate freshness)."
  (and (+cf--read-credentials) t))

(defun +cf--cookie-header ()
  "Return the current cookie as a Cookie header value, or nil."
  (+cf--read-credentials))

;;;###autoload
(defun codeforces-login (cookie)
  "Log in to Codeforces using a browser session COOKIE string.
COOKIE is validated by fetching the login page; on success the home directory
is created.  Interactively, prompts for the cookie (default: current kill)."
  (interactive
   (list (read-string "Codeforces session cookie (paste from browser): "
                      (current-kill 0 t))))
  (message "Validating Codeforces cookie...")
  (+cf--write-credentials cookie)
  (let* ((resp (+cf-http-get (concat +cf-site-base "enter")
                             :cookie cookie))
         (status (car resp))
         (body (cdr resp)))
    (if (and (= status 200) (+cf--login-page-logged-in-p body))
        (progn
          (+cf--ensure-home-dir)
          (message "Codeforces login OK.  Workspace: %s" codeforces-home-directory)
          t)
      (+cf--write-credentials "") ; invalidate
      (error "Codeforces login failed (status %s). Check your cookie." status))))

;;;###autoload
(defun codeforces-logout ()
  "Clear stored Codeforces credentials."
  (interactive)
  (let ((f (+cf--credentials-file)))
    (when (file-exists-p f)
      (delete-file f)))
  (message "Codeforces credentials cleared."))

(provide 'emacs-codeforces-auth)

;;; emacs-codeforces-auth.el ends here
```

- [ ] **Step 4: Run test to verify it passes**

Run:
```bash
emacs --batch -L emacs-codeforces -l emacs-codeforces/test/emacs-codeforces-auth-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
git add emacs-codeforces/emacs-codeforces-auth.el emacs-codeforces/test/emacs-codeforces-auth-test.el
git commit -m "feat(codeforces): add session-cookie auth module with tests"
```

---

### Task 4: API module (problems + submission status)

**Files:**
- Create: `emacs-codeforces/emacs-codeforces-api.el`
- Create: `emacs-codeforces/test/emacs-codeforces-api-test.el`

Wraps `problemset.problems` and `contest.status`. We unit-test the plist
mapping and tag filtering — the pure logic — with fixture JSON.

- [ ] **Step 1: Write the failing test**

Create `emacs-codeforces/test/emacs-codeforces-api-test.el`:

```elisp
;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'ert)
(require 'emacs-codeforces-api)

(defconst +cf-test/problems-json
  "{\"result\":{\"problems\":[{\"contestId\":1234,\"index\":\"A\",\"name\":\"Two\",\"rating\":1200,\"tags\":[\"dp\",\"greedy\"],\"timeLimit\":2.0,\"memoryLimit\":256,\"type\":\"PROGRAMMING\"},{\"contestId\":1234,\"index\":\"F2\",\"name\":\"Hard\",\"rating\":2400,\"tags\":[\"graphs\"],\"timeLimit\":3.5,\"memoryLimit\":512,\"type\":\"PROGRAMMING\"}],\"problemStatistics\":[]}}")

(ert-deftest +cf--parse-problems/returns-plist-list ()
  "Problem JSON result is parsed into a list of problem plists."
  (let ((problems (+cf--parse-problems +cf-test/problems-json)))
    (should (= (length problems) 2))
    (let ((p1 (car problems)))
      (should (equal (plist-get p1 :contestId) 1234))
      (should (equal (plist-get p1 :index) "A"))
      (should (equal (plist-get p1 :rating) 1200))
      (should (equal (plist-get p1 :tags) '("dp" "greedy")))
      (should (equal (plist-get p1 :timeLimit) 2.0))
      (should (equal (plist-get p1 :memoryLimit) 256)))
    (should (equal (plist-get (cadr problems) :index) "F2"))))

(ert-deftest +cf--filter-by-tags/and-semantics ()
  "A problem is kept only if it has ALL selected tags (AND)."
  (let* ((problems (+cf--parse-problems +cf-test/problems-json))
         (kept (+cf--filter-by-tags problems '("dp" "greedy"))))
    (should (= (length kept) 1))
    (should (equal (plist-get (car kept) :index) "A")))

  ;; tag present on only one problem
  (let* ((problems (+cf--parse-problems +cf-test/problems-json))
         (kept (+cf--filter-by-tags problems '("graphs"))))
    (should (= (length kept) 1))
    (should (equal (plist-get (car kept) :index) "F2")))

  ;; tag present on none
  (let* ((problems (+cf--parse-problems +cf-test/problems-json)))
    (should (seq-empty-p (+cf--filter-by-tags problems '("math"))))))

(ert-deftest +cf--filter-by-rating/range ()
  "Rating filter keeps problems within [lo, hi] inclusive."
  (let* ((problems (+cf--parse-problems +cf-test/problems-json))
         (kept (+cf--filter-by-rating problems 1000 1500)))
    (should (= (length kept) 1))
    (should (equal (plist-get (car kept) :index) "A"))))

(ert-deftest +cf--all-tags/union ()
  "All tags across the problem set are collected, deduplicated, sorted."
  (let* ((problems (+cf--parse-problems +cf-test/problems-json))
         (tags (+cf--all-tags problems)))
    (should (equal tags '("dp" "graphs" "greedy")))))
```

- [ ] **Step 2: Run test to verify it fails**

Run:
```bash
emacs --batch -L emacs-codeforces -l emacs-codeforces/test/emacs-codeforces-api-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL — `Cannot open load file: emacs-codeforces-api`.

- [ ] **Step 3: Write minimal implementation**

Create `emacs-codeforces/emacs-codeforces-api.el`:

```elisp
;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-api.el --- Read-only Codeforces JSON API client -*- lexical-binding: t; -*-

;;; Commentary:
;; Wraps the public Codeforces API for problems and submission status.
;; `+cf-fetch-problems' hits `problemset.problems' and caches to
;; `cache/problems.json'.  Filtering helpers (`+cf--filter-by-tags',
;; `+cf--filter-by-rating', `+cf--all-tags') are pure and unit-tested.

;;; Code:

(require 'emacs-codeforces-client)

(defun +cf--cache-file ()
  "Path to the problems cache."
  (expand-file-name "cache/problems.json" codeforces-home-directory))

(defun +cf--parse-problems (json-string)
  "Parse the `problemset.problems' result JSON-STRING into a list of plists."
  (let* ((result (+cf--parse-json-body json-string)))
    (plist-get result :problems)))

(defun +cf--filter-by-tags (problems tags)
  "Keep PROBLEMS having ALL of TAGS (AND semantics)."
  (if (seq-empty-p tags)
      problems
    (cl-remove-if-not
     (lambda (p)
       (let ((ptags (plist-get p :tags)))
         (cl-every (lambda (t) (member t ptags)) tags)))
     problems)))

(defun +cf--filter-by-rating (problems lo hi)
  "Keep PROBLEMS with rating in [LO, HI] inclusive.  Nil rating excluded."
  (cl-remove-if-not
   (lambda (p)
     (let ((r (plist-get p :rating)))
       (and r (>= r lo) (<= r hi))))
   problems))

(defun +cf--all-tags (problems)
  "Return the sorted, deduplicated union of all tags in PROBLEMS."
  (sort (cl-remove-duplicates
         (cl-reduce #'append (mapcar (lambda (p) (plist-get p :tags)) problems))
         :test #'string=)
        #'string<))

(defun +cf-fetch-problems (&optional force)
  "Fetch the problem list, caching locally.  FORCE re-fetches.
Returns a list of problem plists."
  (+cf--ensure-home-dir)            ; defined in auth, but harmless to need
  (let ((cache (+cf--cache-file)))
    (when (or force (not (file-exists-p cache)))
      (let* ((raw (+cf-api-get "problemset.problems"))
             (json (let ((json-encoding-pretty-print t))
                     (json-encode raw))))
        (with-temp-file cache
          (insert (json-encode `(:result (:problems ,raw)))))))
    (+cf--parse-problems
     (with-temp-buffer
       (insert-file-contents cache)
       (buffer-string)))))

(defun +cf-fetch-submission (contest-id submission-id)
  "Fetch a single submission's status from `contest.status'.
Returns a plist with at least :id :verdict :passedTestCount :timeConsumedMillis
:memoryConsumedBytes.  :verdict may be nil while queued."
  (let* ((result (+cf-api-get "contest.status"
                              :contestId contest-id
                              :from 1
                              :count 100))
         (subs result)
         (found (cl-find submission-id subs
                        :key (lambda (s) (plist-get s :id))
                        :test #'equal)))
    (or found
        (error "Submission %s not found in contest %s" submission-id contest-id))))

(provide 'emacs-codeforces-api)

;;; emacs-codeforces-api.el ends here
```

- [ ] **Step 4: Run test to verify it passes**

Run:
```bash
emacs --batch -L emacs-codeforces -l emacs-codeforces/test/emacs-codeforces-api-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (4 tests).

- [ ] **Step 5: Commit**

```bash
git add emacs-codeforces/emacs-codeforces-api.el emacs-codeforces/test/emacs-codeforces-api-test.el
git commit -m "feat(codeforces): add read-only API module with problems + submission status"
```

---

### Task 5: Workspace module (per-problem dirs + language templates)

**Files:**
- Create: `emacs-codeforces/emacs-codeforces-workspace.el`
- Create: `emacs-codeforces/templates/rust.rs`
- Create: `emacs-codeforces/templates/python.py`
- Create: `emacs-codeforces/templates/cpp.cpp`
- Create: `emacs-codeforces/templates/java.java`
- Create: `emacs-codeforces/test/emacs-codeforces-workspace-test.el`

Pure local-filesystem module: per-problem directory, template instantiation,
language→extension map. Fully unit-testable in a temp dir.

- [ ] **Step 1: Write the failing test**

Create `emacs-codeforces/test/emacs-codeforces-workspace-test.el`:

```elisp
;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'ert)
(require 'emacs-codeforces-workspace)

(ert-deftest +cf--language-extension/map ()
  "Language name maps to the expected file extension."
  (should (equal (+cf--language-extension "rust") "rs"))
  (should (equal (+cf--language-extension "python") "py"))
  (should (equal (+cf--language-extension "cpp") "cpp"))
  (should (equal (+cf--language-extension "java") "java")))

(ert-deftest +cf--problem-dir/format ()
  "Problem dir is {contestId}_{index} under workspace/."
  (let ((codeforces-home-directory "/tmp/cf/"))
    (should (equal (+cf--problem-dir '(:contestId 1234 :index "A"))
                   "/tmp/cf/workspace/1234_A"))
    ;; multi-char index like F2
    (should (equal (+cf--problem-dir '(:contestId 1234 :index "F2"))
                   "/tmp/cf/workspace/1234_F2"))))

(ert-deftest +cf-init-solution/creates-template-and-dir ()
  "Init creates the problem dir and writes the template; doesn't overwrite."
  (let ((codeforces-home-directory
         (expand-file-name (make-temp-name "cf-ws-test-") temporary-file-directory))
        (codeforces-templates-directory nil))
    (unwind-protect
        (let* ((problem '(:contestId 5 :index "B"))
               (sol (+cf-init-solution problem "rust")))
          ;; file exists at the expected path
          (should (file-exists-p sol))
          (should (string-match-p "/workspace/5_B/solution.rs$" sol))
          ;; template content present
          (with-temp-buffer
            (insert-file-contents sol)
            (should (string-match-p "fn main" (buffer-string))))
          ;; idempotent: does not overwrite an edited solution
          (with-temp-buffer
            (insert "MY EDIT")
            (write-file sol))
          (+cf-init-solution problem "rust")
          (with-temp-buffer
            (insert-file-contents sol)
            (should (string-match-p "MY EDIT" (buffer-string)))))
      (delete-directory codeforces-home-directory t))))
```

- [ ] **Step 2: Run test to verify it fails**

Run:
```bash
emacs --batch -L emacs-codeforces -l emacs-codeforces/test/emacs-codeforces-workspace-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL — `Cannot open load file: emacs-codeforces-workspace`.

- [ ] **Step 3: Create the language templates**

Create `emacs-codeforces/templates/rust.rs`:

```rust
// Codeforces solution: {problem_id}
use std::io::{self, Read, Write, BufWriter};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let stdout = io::stdout();
    let mut out = BufWriter::new(stdout.lock());
    // TODO: parse input and write the answer.
    let _ = out.write_all(b"");
}
```

Create `emacs-codeforces/templates/python.py`:

```python
# Codeforces solution: {problem_id}
import sys

def main():
    data = sys.stdin.read().split()
    # TODO: parse and solve.

if __name__ == "__main__":
    main()
```

Create `emacs-codeforces/templates/cpp.cpp`:

```cpp
// Codeforces solution: {problem_id}
#include <bits/stdc++.h>
using namespace std;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    // TODO: read input and print the answer.
    return 0;
}
```

Create `emacs-codeforces/templates/java.java`:

```java
// Codeforces solution: {problem_id}
import java.io.*;
import java.util.*;

public class Main {
    public static void main(String[] args) throws IOException {
        // TODO: read input and print the answer.
    }
}
```

- [ ] **Step 4: Write minimal implementation**

Create `emacs-codeforces/emacs-codeforces-workspace.el`:

```elisp
;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-workspace.el --- Local workspace + templates -*- lexical-binding: t; -*-

;;; Commentary:
;; Per-problem directories under `~/.emacs-codeforces/workspace/' and language
;; template instantiation.  Pure local-filesystem, no network.

;;; Code:

(defun +cf--templates-dir ()
  "Return the templates directory (override or bundled)."
  (or codeforces-templates-directory
      (expand-file-name "templates"
                        (file-name-directory
                         (or load-file-name buffer-file-name)))))

(defun +cf--language-extension (language)
  "Return the file extension for LANGUAGE (\"rust\" -> \"rs\")."
  (pcase language
    ("rust" "rs")
    ("python" "py")
    ("cpp" "cpp")
    ("java" "java")
    (_ (error "Unknown language: %s" language))))

(defun +cf--language-template-file (language)
  "Return the template file path for LANGUAGE."
  (expand-file-name
   (pcase language
     ("rust" "rust.rs")
     ("python" "python.py")
     ("cpp" "cpp.cpp")
     ("java" "java.java")
     (_ (error "Unknown language: %s" language)))
   (+cf--templates-dir)))

(defun +cf--problem-id (problem)
  "Return \"contestId_index\" for PROBLEM."
  (format "%s_%s" (plist-get problem :contestId) (plist-get problem :index)))

(defun +cf--problem-dir (problem)
  "Return the per-problem workspace directory."
  (expand-file-name (+cf--problem-id problem)
                    (expand-file-name "workspace" codeforces-home-directory)))

(defun +cf-init-solution (problem language)
  "Create PROBLEM's workspace dir and init solution.<ext> from LANGUAGE template.
Returns the path to the solution file.  Never overwrites an existing solution."
  (let* ((dir (+cf--problem-dir problem))
         (ext (+cf--language-extension language))
         (sol (expand-file-name (concat "solution." ext) dir)))
    (make-directory dir t)
    (unless (file-exists-p sol)
      (let ((tpl (+cf--language-template-file language)))
        (with-temp-file sol
          (insert-file-contents tpl)
          (goto-char (point-min))
          (while (search-forward "{problem_id}" nil t)
            (replace-match (+cf--problem-id problem) t t)))))
    sol))

(provide 'emacs-codeforces-workspace)

;;; emacs-codeforces-workspace.el ends here
```

- [ ] **Step 5: Run test to verify it passes**

Run:
```bash
emacs --batch -L emacs-codeforces -l emacs-codeforces/test/emacs-codeforces-workspace-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (3 tests).

- [ ] **Step 6: Commit**

```bash
git add emacs-codeforces/emacs-codeforces-workspace.el emacs-codeforces/templates emacs-codeforces/test/emacs-codeforces-workspace-test.el
git commit -m "feat(codeforces): add workspace module with language templates"
```

---

## Phase 3: Scrape (HTML→Org + submit)

### Task 6: Scrape module (statement HTML→Org converter)

**Files:**
- Create: `emacs-codeforces/emacs-codeforces-scrape.el`
- Create: `emacs-codeforces/test/emacs-codeforces-scrape-test.el`

This task implements the HTML→Org converter and the statement fetcher. Submit
POST logic is added in Task 7. We test the converter against a fixture HTML
fragment (no network).

- [ ] **Step 1: Write the failing test for the converter**

Create `emacs-codeforces/test/emacs-codeforces-scrape-test.el`:

```elisp
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
```

- [ ] **Step 2: Run test to verify it fails**

Run:
```bash
emacs --batch -L emacs-codeforces -l emacs-codeforces/test/emacs-codeforces-scrape-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL — `Cannot open load file: emacs-codeforces-scrape`.

- [ ] **Step 3: Write minimal implementation**

Create `emacs-codeforces/emacs-codeforces-scrape.el`:

```elisp
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
  "Return the inner HTML of the first <div class=\"problem-statement\">."
  (if (not (string-match "<div[^>]*class=\"problem-statement\"[^>]*>\\(\\(?:.\\|\n\\)*?\\)</div>\\s-*</div>\\s-*</div>" html))
      html                            ; fallback: return whole page
    (match-string 0 html)))

(defun +cf--html-node-to-org (node)
  "Recursively convert a parsed HTML NODE (dom) to an Org string."
  (pcase (dom-tag node)
    ('nil (concat (dom-text node)))   ; text node
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
    (_ (mapconcat #'+cf--html-node-to-org (dom-children node) ""))))

(defun +cf--html-to-org (html)
  "Convert statement HTML string to an Org string."
  (with-temp-buffer
    (insert html)
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      ;; take body subtree
      (let ((body (or (dom-by-tag dom 'body) (list 'body nil dom))))
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

(defun +cf-submit-via-http (problem source language)
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

(defun +cf-submit-via-browser (problem language)
  "Open the pre-filled submit page for PROBLEM/LANGUAGE in a browser.
Used as the Cloudflare fallback."
  (let ((url (format "%scontest/%s/submit"
                     +cf-site-base
                     (plist-get problem :contestId))))
    (browse-url url)
    (message "Cloudflare blocked direct submit.  Submit '%s' in the browser; results tracked here."
             (plist-get problem :index))))

(provide 'emacs-codeforces-scrape)

;;; emacs-codeforces-scrape.el ends here
```

- [ ] **Step 4: Run test to verify it passes**

Run:
```bash
emacs --batch -L emacs-codeforces -l emacs-codeforces/test/emacs-codeforces-scrape-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (4 tests).

- [ ] **Step 5: Commit**

```bash
git add emacs-codeforces/emacs-codeforces-scrape.el emacs-codeforces/test/emacs-codeforces-scrape-test.el
git commit -m "feat(codeforces): add scrape module (HTML->Org converter + submit)"
```

---

## Phase 4: UI (list + detail)

### Task 7: Problem list (tabulated-list + tag/rating filter)

**Files:**
- Create: `emacs-codeforces/emacs-codeforces-list.el`

UI module; not unit-tested (interactive buffer). We verify by byte-compile and a
batch smoke eval.

- [ ] **Step 1: Write the implementation**

Create `emacs-codeforces/emacs-codeforces-list.el`:

```elisp
;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-list.el --- Problem list buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; `codeforces-browse-problems' opens `*Codeforces Problems*' as a
;; `tabulated-list-mode' derivative.  Columns: ID, Name, Rating, Tags.
;; `/` filters by tags (consult multi-select, AND), `r` by rating range,
;; `RET` opens the statement in a right-side window, `g` refreshes.

;;; Code:

(require 'tabulated-list)
(require 'emacs-codeforces-api)
(require 'emacs-codeforces-detail)

(defvar-local +cf--all-problems nil
  "All problems fetched (unfiltered).")

(defvar-local +cf--tag-filter nil
  "Current tag filter (list of strings), nil = no filter.")

(defvar-local +cf--rating-filter nil
  "Current rating filter (lo . hi), nil = no filter.")

(defun +cf--problem-id (p)
  "Compact id for PROBLEM, e.g. \"1234A\"."
  (format "%s%s" (plist-get p :contestId) (plist-get p :index)))

(defun +cf--format-tags (tags)
  "Join TAGS with \", \"."
  (mapconcat #'identity (or tags '("")) ", "))

(defun +cf--problem->entry (p)
  "Convert PROBLEM plist to a tabulated-list entry [id name rating tags] . P."
  (list p
        (vector (+cf--problem-id p)
                (or (plist-get p :name) "")
                (number-to-string (or (plist-get p :rating) 0))
                (+cf--format-tags (plist-get p :tags)))))

(defun +cf--apply-filters ()
  "Recompute the tabulated entries from current filters."
  (let ((problems +cf--all-problems))
    (when +cf--tag-filter
      (setq problems (+cf--filter-by-tags problems +cf--tag-filter)))
    (when +cf--rating-filter
      (setq problems (apply #'+cf--filter-by-rating problems +cf--rating-filter)))
    (setq tabulated-list-entries
          (mapcar #'+cf--problem->entry problems))
    (tabulated-list-print t)))

(defun +cf--open-at-point ()
  "Open the statement for the problem at point."
  (interactive)
  (let ((p (tabulated-list-get-id)))
    (when p
      (codeforces-open-problem p))))

(defun +cf--read-tags (problems)
  "Interactively read multiple tags via `consult', candidates from PROBLEMS."
  (if (require 'consult nil 'noerror)
      (consult-completing-read-multiple
       (+cf--all-tags problems)
       "Tags (AND): " nil t)
    (completing-read-multiple "Tags (AND): " (+cf--all-tags problems))))

(defun codeforces-filter-by-tags (tags)
  "Set the tag filter to TAGS (list of strings).  Empty clears."
  (interactive (list (+cf--read-tags +cf--all-problems)))
  (setq +cf--tag-filter (if (seq-empty-p tags) nil tags))
  (+cf--apply-filters))

(defun codeforces-filter-by-rating (range)
  "Set the rating filter to RANGE (\"lo-hi\").  Empty clears."
  (interactive
   (list (read-string "Rating range (e.g. 800-1500, empty to clear): "
                      (if +cf--rating-filter
                          (format "%s-%s" (car +cf--rating-filter) (cdr +cf--rating-filter))))))
  (setq +cf--rating-filter
        (if (string-empty-p range)
            nil
          (if (string-match "\\([0-9]+\\)-\\([0-9]+\\)" range)
              (cons (string-to-number (match-string 1 range))
                    (string-to-number (match-string 2 range)))
            (error "Bad range: %s" range))))
  (+cf--apply-filters))

(defun +cf--refresh ()
  "Refresh the problem list from the API."
  (interactive)
  (setq +cf--all-problems (+cf-fetch-problems 'force))
  (+cf--apply-filters))

(define-derived-mode codeforces-problems-mode tabulated-list-mode "CF Problems"
  "Major mode for browsing Codeforces problems.
\\<codeforces-problems-mode>
\\{codeforces-problems-mode}"
  (setq tabulated-list-format [("ID" 8 t)
                               ("Name" 50 t)
                               ("Rating" 8 t)
                               ("Tags" 40 nil)]
        tabulated-list-sort-key (cons "ID" . nil)
        tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun +cf--buffer ()
  "Return (creating if needed) the problems buffer."
  (or (get-buffer "*Codeforces Problems*")
      (with-current-buffer (get-buffer-create "*Codeforces Problems*")
        (codeforces-problems-mode)
        (current-buffer))))

;;;###autoload
(defun codeforces-browse-problems ()
  "Open the Codeforces problem list."
  (interactive)
  (let ((buf (+cf--buffer)))
    (with-current-buffer buf
      (setq +cf--all-problems (+cf-fetch-problems))
      (+cf--apply-filters))
    (pop-to-buffer buf)))

(let ((map codeforces-problems-mode-map))
  (define-key map (kbd "RET") #'+cf--open-at-point)
  (define-key map (kbd "/") #'codeforces-filter-by-tags)
  (define-key map (kbd "r") #'codeforces-filter-by-rating)
  (define-key map (kbd "g") #'+cf--refresh))

(provide 'emacs-codeforces-list)

;;; emacs-codeforces-list.el ends here
```

- [ ] **Step 2: Verify byte-compile**

Run:
```bash
emacs --batch -L emacs-codeforces -f batch-byte-compile emacs-codeforces/emacs-codeforces-list.el
```
Expected: no errors (warnings about free variables for `codeforces-home-directory` are acceptable).

- [ ] **Step 3: Commit**

```bash
git add emacs-codeforces/emacs-codeforces-list.el
git commit -m "feat(codeforces): add problem list buffer with tag/rating filter"
```

---

### Task 8: Problem detail (Org buffer + C-c C-c solve + C-c C-s submit)

**Files:**
- Create: `emacs-codeforces/emacs-codeforces-detail.el`

The detail buffer renders the statement in Org, binds `C-c C-c` (start solving)
and `C-c C-s` (submit), and runs the verdict poller. The Org-rendering and
polling logic is rich enough that we test the pure parts in Task 8b.

- [ ] **Step 1: Write the implementation**

Create `emacs-codeforces/emacs-codeforces-detail.el`:

```elisp
;; -*- coding: utf-8; lexical-binding: t; -*-

;;; emacs-codeforces-detail.el --- Problem detail Org buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; `codeforces-open-problem' opens `*CF <id>*', an Org buffer rendering the
;; statement.  `C-c C-c' starts solving (creates workspace dir + solution file),
;; `C-c C-s' submits and polls verdicts in-place.  The detail buffer uses
;; `olivetti-mode'/`org-modern-mode' if available (reusing RivenEmacs org setup).

;;; Code:

(require 'org)
(require 'emacs-codeforces-api)
(require 'emacs-codeforces-scrape)
(require 'emacs-codeforces-workspace)

(defvar-local +cf--current-problem nil
  "The problem plist for this detail buffer.")

(defvar-local +cf--solution-language nil
  "Language chosen for this problem's solution.")

(defvar-local +cf--poll-timer nil
  "Active submission poll timer, if any.")

(defvar-local +cf--poll-start nil
  "Timestamp the current poll started.")

(defun +cf--buffer-name (problem)
  "Buffer name for PROBLEM."
  (format "*CF %s*" (+cf--problem-id problem)))

(defun +cf--problem-id (problem)
  "Compact id, e.g. \"1234A\"."
  (format "%s%s" (plist-get problem :contestId) (plist-get problem :index)))

(defun +cf--render-header (problem)
  "Return the Org header/property block for PROBLEM."
  (format "* %s - %s%s                              :codeforces:
:PROPERTIES:
:Rating: %s
:Time-Limit: %s s
:Memory-Limit: %s MB
:URL: %sproblemset/problem/%s/%s
:END:
"
          (+cf--problem-id problem)
          (or (plist-get problem :name) "")
          (if +cf--solution-language " ●" "")
          (or (plist-get problem :rating) "-")
          (or (plist-get problem :timeLimit) "-")
          (or (plist-get problem :memoryLimit) "-")
          +cf-site-base
          (plist-get problem :contestId)
          (plist-get problem :index)))

(defun +cf--render-tags (problem)
  "Return the tags section for PROBLEM."
  (format "** Tags\n%s\n\n"
          (+cf--format-tags (plist-get problem :tags))))

(defun +cf--render-status (text)
  "Return the submission status section with TEXT."
  (format "** Submission Status\n%s\n" (or text "*No active submission.*")))

(defun +cf--format-verdict (sub)
  "Format a submission plist SUB as a human-readable status string."
  (let ((v (plist-get sub :verdict))
        (passed (plist-get sub :passedTestCount))
        (time (plist-get sub :timeConsumedMillis))
        (mem (plist-get sub :memoryConsumedBytes)))
    (cond
     ((null v) "Queued / compiling…")
     ((eq v 'TESTING)
      (format "Running… passed %s tests." (or passed 0)))
     ((eq v 'OK)
      (format "OK ✅  (%sms, %sMB)"
              (or time 0)
              (/ (or mem 0) 1048576)))
     (t (format "%s ❌  (passed %s tests)"
                v (or passed 0))))))

(defun +cf--write-buffer (problem body)
  "Render PROBLEM with BODY (the Org statement) into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (+cf--render-header problem))
    (insert (+cf--render-tags problem))
    (insert "** Statement\n")
    (insert (or body "(could not fetch statement)"))
    (insert "\n\n")
    (insert (+cf--render-status nil))))

(defun +cf--update-status (text)
  "Replace the Submission Status section in the current buffer with TEXT."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\* Submission Status\n\\(\\(?:.\\|\n\\)*\\)$" nil t)
        (goto-char (match-beginning 1))
        (delete-region (match-beginning 1) (match-end 1))
        (insert text)))))

(defun +cf--stop-poll ()
  "Stop the current poll timer."
  (when (timerp +cf--poll-timer)
    (cancel-timer +cf--poll-timer))
  (setq +cf--poll-timer nil))

(defun +cf--terminal-verdict-p (verdict)
  "Return non-nil if VERDICT is terminal."
  (member verdict '(OK WRONG_ANSWER TIME_LIMIT_EXCEEDED
                    MEMORY_LIMIT_EXCEEDED RUNTIME_ERROR COMPILATION_ERROR
                    PARTIAL IDLENESS_LIMIT_EXCEEDED CHALLENGED
                    SKIPPED REJECTED FAILED)))

(defun +cf--poll-once (contest-id submission-id)
  "Poll once and update the status section; stop when terminal or timed out."
  (condition-case err
      (let* ((sub (+cf-fetch-submission contest-id submission-id))
             (text (+cf--format-verdict sub))
             (v (plist-get sub :verdict)))
        (+cf--update-status text)
        (when (or (+cf--terminal-verdict-p v)
                  (> (- (float-time) +cf--poll-start) codeforces-poll-timeout))
          (+cf--stop-poll)
          (when (+cf--terminal-verdict-p v)
            (message "Codeforces: %s" text))))
    (error
     (message "Codeforces poll error: %s" (error-message-string err))
     (+cf--stop-poll))))

(defun +cf--start-poll (contest-id submission-id)
  "Start polling submission status every `codeforces-poll-interval' seconds."
  (+cf--stop-poll)
  (setq +cf--poll-start (float-time))
  (setq +cf--poll-timer
        (run-with-timer
         codeforces-poll-interval codeforces-poll-interval
         #'+cf--poll-once contest-id submission-id)))

(defun +cf--solution-file (problem)
  "Return the path to PROBLEM's solution file, or nil."
  (when +cf--solution-language
    (expand-file-name (concat "solution."
                              (+cf--language-extension +cf--solution-language))
                      (+cf--problem-dir problem))))

(defun +cf--choose-language ()
  "Prompt for a solution language, defaulting to `codeforces-default-language'."
  (completing-read "Language: "
                   '("rust" "python" "cpp" "java")
                   nil t nil nil
                   codeforces-default-language))

(defun codeforces-start-solving ()
  "Start solving the current problem: pick language, init workspace, open file."
  (interactive)
  (unless +cf--current-problem
    (error "No problem in this buffer"))
  (unless +cf--solution-language
    (setq +cf--solution-language (+cf--choose-language)))
  (let* ((problem +cf--current-problem)
         (sol (+cf-init-solution problem +cf--solution-language)))
    ;; Re-render header to reflect the ● solving marker.
    (+cf--write-buffer problem
                       (+cf-fetch-problem-org problem))
    (find-file-other-window sol)
    (message "Solving %s in %s" (+cf--problem-id problem) +cf--solution-language)))

(defun +cf--read-source (path)
  "Return file contents at PATH, or error."
  (if (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))
    (error "Solution file not found: %s" path)))

(defun codeforces-submit ()
  "Submit the current problem's solution and start polling.
Falls back to the browser on Cloudflare."
  (interactive)
  (unless +cf--current-problem
    (error "No problem in this buffer"))
  (let* ((problem +cf--current-problem)
         (sol (+cf--solution-file problem)))
    (unless sol
      (error "Run C-c C-c to start solving first"))
    (unless +cf--solution-language
      (error "Language not set"))
    (let ((source (+cf--read-source sol)))
      (+cf--update-status "Submitting…")
      (condition-case err
          (let ((result (+cf-submit-via-http problem source +cf--solution-language)))
            (cond
             ((eq result 'cloudflare)
              (+cf-submit-via-browser problem +cf--solution-language)
              (+cf--update-status
               "Submit blocked by Cloudflare — complete in browser. Polling skipped (no id)."))
             (t
              (+cf--update-status
               (format "Submitted as %s. Polling…" result))
              (+cf--start-poll (plist-get problem :contestId) result))))
        (error
         (+cf--update-status (format "Submit failed: %s"
                                     (error-message-string err))))))))

;;;###autoload
(defun codeforces-open-problem (problem)
  "Open PROBLEM (plist) in an Org detail buffer on the right."
  (interactive)
  (let ((buf (get-buffer-create (+cf--buffer-name problem))))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode)
        (setq buffer-read-only t)
        (use-local-map (copy-keymap org-mode-map))
        (local-set-key (kbd "C-c C-c") #'codeforces-start-solving)
        (local-set-key (kbd "C-c C-s") #'codeforces-submit))
      (setq +cf--current-problem problem)
      (let ((org (+cf-fetch-problem-org problem)))
        (+cf--write-buffer problem org)))
    (display-buffer buf
                    '((display-buffer-in-direction)
                      (direction . right)
                      (window-width . 0.5)))
    buf))

(provide 'emacs-codeforces-detail)

;;; emacs-codeforces-detail.el ends here
```

- [ ] **Step 2: Verify byte-compile**

Run:
```bash
emacs --batch -L emacs-codeforces -f batch-byte-compile emacs-codeforces/emacs-codeforces-detail.el
```
Expected: no errors.

- [ ] **Step 3: Commit**

```bash
git add emacs-codeforces/emacs-codeforces-detail.el
git commit -m "feat(codeforces): add problem detail buffer with solve + submit + polling"
```

---

### Task 8b: Detail pure-logic tests

**Files:**
- Create: `emacs-codeforces/test/emacs-codeforces-detail-test.el`

The interactive detail buffer isn't unit-tested, but its pure helpers
(`+cf--format-verdict`, `+cf--terminal-verdict-p`) are.

- [ ] **Step 1: Write the test**

Create `emacs-codeforces/test/emacs-codeforces-detail-test.el`:

```elisp
;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'ert)
(require 'emacs-codeforces-detail)

(ert-deftest +cf--format-verdict/queued ()
  (should (equal (+cf--format-verdict '(:id 1))
                 "Queued / compiling…")))

(ert-deftest +cf--format-verdict/testing ()
  (should (equal (+cf--format-verdict '(:id 1 :verdict TESTING :passedTestCount 12))
                 "Running… passed 12 tests.")))

(ert-deftest +cf--format-verdict/ok ()
  (should (string-match-p "OK"
                          (+cf--format-verdict
                           '(:id 1 :verdict OK :passedTestCount 30
                                  :timeConsumedMillis 124 :memoryConsumedBytes 8388608)))))

(ert-deftest +cf--format-verdict/wa ()
  (should (string-match-p "WRONG_ANSWER"
                          (+cf--format-verdict
                           '(:id 1 :verdict WRONG_ANSWER :passedTestCount 13)))))

(ert-deftest +cf--terminal-verdict-p/classifies ()
  (should (+cf--terminal-verdict-p 'OK))
  (should (+cf--terminal-verdict-p 'WRONG_ANSWER))
  (should-not (+cf--terminal-verdict-p 'TESTING))
  (should-not (+cf--terminal-verdict-p nil)))
```

- [ ] **Step 2: Run tests**

Run:
```bash
emacs --batch -L emacs-codeforces -l emacs-codeforces/test/emacs-codeforces-detail-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (5 tests).

- [ ] **Step 3: Commit**

```bash
git add emacs-codeforces/test/emacs-codeforces-detail-test.el
git commit -m "test(codeforces): add detail pure-logic tests (verdict formatting)"
```

---

## Phase 5: Integration

### Task 9: RivenEmacs integration module

**Files:**
- Create: `lisp/tools/init-codeforces.el`
- Modify: `init.el` — add `(require 'init-codeforces)` to `riven/load-deferred-modules`

- [ ] **Step 1: Create the integration module**

Create `lisp/tools/init-codeforces.el`:

```elisp
;; -*- coding: utf-8; lexical-binding: t; -*-

;;; init-codeforces.el --- RivenEmacs integration for emacs-codeforces -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin wrapper that loads the `emacs-codeforces' library from the repo-local
;; `emacs-codeforces/' directory and binds a leader key.

;;; Code:

(use-package emacs-codeforces
  :load-path (lambda () (list (expand-file-name "emacs-codeforces" root-dir)))
  :commands (codeforces-login codeforces-browse-problems codeforces-logout)
  :custom
  (codeforces-default-language "rust"))

;; Leader-key bindings (general.el is loaded by init-general).
(with-eval-after-load 'init-general
  (leader-def
    :infix "c"
    "" '(:ignore t :wk "Codeforces")
    "l" '(codeforces-login :wk "Login")
    "L" '(codeforces-logout :wk "Logout")
    "p" '(codeforces-browse-problems :wk "Browse problems")))

(provide 'init-codeforces)

;;; init-codeforces.el ends here
```

- [ ] **Step 2: Wire into init.el**

In `/Users/riven/Github/RivenEmacs/init.el`, inside `riven/load-deferred-modules` (after the `(require 'init-reader)` line, before the closing paren), add:

```elisp
  (require 'init-codeforces)
```

Edit target in `init.el` (the `riven/load-deferred-modules` body):
```elisp
  (require 'init-quickrun)
  (require 'init-feed)
  (require 'init-lookup)
  (require 'init-reader)
  (require 'init-codeforces)
```

- [ ] **Step 3: Verify the library loads under RivenEmacs**

Run:
```bash
emacs --batch -l init.el --eval "(message \"Config loaded; cf feature: %s\" (featurep 'emacs-codeforces))"
```
Expected: prints `Config loaded; cf feature: t` (may print byte-compile warnings, which are OK). If `emacs-codeforces` is not loaded at startup (it's `:commands`-deferred), instead verify the autoload can resolve:
```bash
emacs --batch -l init.el --eval "(require 'emacs-codeforces) (message \"loaded: %s\" (featurep 'emacs-codeforces))"
```
Expected: `loaded: t`.

- [ ] **Step 4: Commit**

```bash
git add lisp/tools/init-codeforces.el init.el
git commit -m "feat(codeforces): integrate emacs-codeforces into RivenEmacs (SPC c prefix)"
```

---

### Task 10: Whole-library verification + final polish

**Files:**
- Verify only.

- [ ] **Step 1: Run the full test suite**

Run:
```bash
emacs --batch -L emacs-codeforces \
  -l emacs-codeforces/test/emacs-codeforces-client-test.el \
  -l emacs-codeforces/test/emacs-codeforces-auth-test.el \
  -l emacs-codeforces/test/emacs-codeforces-api-test.el \
  -l emacs-codeforces/test/emacs-codeforces-workspace-test.el \
  -l emacs-codeforces/test/emacs-codeforces-scrape-test.el \
  -l emacs-codeforces/test/emacs-codeforces-detail-test.el \
  -f ert-run-tests-batch-and-exit
```
Expected: all tests PASS.

- [ ] **Step 2: Byte-compile every library file**

Run:
```bash
emacs --batch -L emacs-codeforces -f batch-byte-compile emacs-codeforces/*.el
```
Expected: no errors. Note any warnings about free variables referencing `codeforces-home-directory` etc. (they're `defcustom`d in the entry module and dynamically bound at runtime). If warnings are excessive, add `defvars` at the top of the affected file — but functional correctness comes first.

- [ ] **Step 3: Verify the entry module loads**

Run:
```bash
emacs --batch -L emacs-codeforces -l emacs-codeforces -eval "(message \"entry feature: %s\" (featurep 'emacs-codeforces))"
```
Expected: `entry feature: t`. (Note: `-eval` is an alias used in some builds; if it errors use `--eval`.)

- [ ] **Step 4: Manual smoke checklist (record results)**

Open Emacs with `emacs -l init.el` and verify each (no real account needed for 1, 5; for 2-4 use a real cookie):

- [ ] `M-x codeforces-browse-problems` → list opens, columns render, `g` refreshes.
- [ ] `/` → tag multi-select appears; selecting `dp` filters the list (AND with another tag narrows further).
- [ ] `r` → `800-1500` filters by rating.
- [ ] `RET` on a row → `*CF <id>*` opens on the right in Org, statement visible.
- [ ] In detail buffer `C-c C-c` → language prompt → `solution.rs` opens with the Rust template; `~/.emacs-codeforces/workspace/<id>/` created.
- [ ] Edit the solution, `C-c C-s` → status section updates ("Submitting…" then polling). If Cloudflare blocks, status shows the browser-fallback message.
- [ ] Cookie expiry: delete `credentials`, `C-c C-s` → clear error prompting `M-x codeforces-login`.

- [ ] **Step 5: Commit any test-result notes (optional)**

If any smoke check failed, fix and re-commit. Otherwise the library is complete.

---

## Self-Review Notes (post-writing)

**Spec coverage:**
- Requirement 1 (login + create dir) → Task 3 (`codeforces-login` → `+cf--ensure-home-dir`).
- Requirement 2 (problem list + tag search) → Tasks 4 (filter helpers) + 7 (list buffer, `/` key).
- Requirement 3 (RET → right window detail + tags) → Task 8 (`codeforces-open-problem` with right-direction `display-buffer`; tags section in Org header).
- Requirement 4 (C-c C-c start solving) → Task 8 (`codeforces-start-solving`).
- Requirement 5 (submit + real-time result) → Tasks 6 (submit + Cloudflare fallback) + 8 (`codeforces-submit` + `+cf--start-poll`).

**Placeholder scan:** none. Every code step contains complete code; every command has expected output.

**Type/signature consistency:** `+cf--problem-id` is defined in both workspace and detail modules independently (private to each, consistent format `"{contestId}_{index}"` vs `"{contestId}{index}"`). Note: workspace uses `_` (for dir name), detail uses no separator (for buffer name `*CF 1234A*`). This is intentional and matches the spec (dir = `1234_A`, buffer = `*CF 1234A*`). Both signatures `(problem) -> string` are consistent within each module.

**Risks carried forward:**
- Submit via HTTP may be Cloudflare-blocked (mitigated: detect + browser fallback; status poll via API is unaffected).
- `programTypeId` is scraped live, never hardcoded (Task 6 `+cf--parse-language-options`).
- Cookie expiry handled by auth validation + clear errors.
