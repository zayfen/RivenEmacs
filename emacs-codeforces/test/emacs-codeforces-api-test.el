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
  (let ((problems (+cf--parse-problems +cf-test/problems-json)))
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

;;; Cache safety

(require 'cl-lib)

(defmacro +cf-api-test--with-temp-home (&rest body)
  "Evaluate BODY with a fresh private `codeforces-home-directory'."
  (declare (indent 0) (debug t))
  `(let ((codeforces-home-directory
          (expand-file-name (make-temp-name "cf-api-") temporary-file-directory)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p codeforces-home-directory)
         (delete-directory codeforces-home-directory t)))))

(defconst +cf-api-test/valid-response
  "{\"status\":\"OK\",\"result\":{\"problems\":[{\"contestId\":1,\"index\":\"A\",\"name\":\"First\",\"tags\":[\"implementation\"]}]}}"
  "Minimal valid `problemset.problems' response.")

(ert-deftest +cf-fetch-problems/does-not-cache-http-error ()
  "An HTTP failure must not create or overwrite the problems cache."
  (+cf-api-test--with-temp-home
    (cl-letf (((symbol-function '+cf-http-get)
               (lambda (&rest _) (cons 500 "<html>boom</html>"))))
      (should-error (+cf-fetch-problems 'force))
      (should-not (file-exists-p (+cf--cache-file))))))

(ert-deftest +cf-fetch-problems/does-not-cache-invalid-json ()
  "A non-JSON response must not be written to the problems cache."
  (+cf-api-test--with-temp-home
    (cl-letf (((symbol-function '+cf-http-get)
               (lambda (&rest _) (cons 200 "<html>not json</html>"))))
      (should-error (+cf-fetch-problems 'force))
      (should-not (file-exists-p (+cf--cache-file))))))

(ert-deftest +cf-fetch-problems/does-not-cache-empty-problem-list ()
  "A syntactically valid response with no problems is refused, not cached."
  (+cf-api-test--with-temp-home
    (cl-letf (((symbol-function '+cf-http-get)
               (lambda (&rest _)
                 (cons 200 "{\"status\":\"OK\",\"result\":{\"problems\":[]}}"))))
      (should-error (+cf-fetch-problems 'force))
      (should-not (file-exists-p (+cf--cache-file))))))

(ert-deftest +cf-fetch-problems/caches-valid-response-atomically ()
  "A valid response is cached and parsed, leaving no temporary file behind."
  (+cf-api-test--with-temp-home
    (cl-letf (((symbol-function '+cf-http-get)
               (lambda (&rest _) (cons 200 +cf-api-test/valid-response))))
      (let ((problems (+cf-fetch-problems 'force)))
        (should (= (length problems) 1))
        (should (equal (plist-get (car problems) :index) "A"))
        (should (file-exists-p (+cf--cache-file)))
        (should-not (directory-files (file-name-directory (+cf--cache-file))
                                     nil "\\`\\.problems-"))))))
