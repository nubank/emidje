;;; emidje-run-facts-and-show-reports-tests --- Tests for Emidje -*- lexical-binding: t -*-

;; Copyright © 2018 Nubank

;; This file is not part of GNU Emacs

;;; Commentary:

;; Tests for Emidje.

;;; Code:

(require 'buttercup)
(require 'emidje)
(require 'emidje-test-helpers)

(describe "When I open a Clojure file and call `emidje-run-ns-tests'"

          (it "infers the test namespace that corresponds to the namespace I'm currently in"
              (spy-on 'emidje-send-test-request)
              (emidje-tests-with-temp-buffer "(ns octocat.math)

(defn pow2 [x]
)"
                                             (emidje-run-ns-tests))
              (expect 'emidje-send-test-request :to-have-been-called-with :ns `(ns "octocat.math-test")))

          (it "does the same when I am in a test namespace"
              (spy-on 'emidje-send-test-request)
              (emidje-tests-with-temp-buffer "(ns octocat.math-test)
(fact (math/pow2 2) => 4)"
                                             (emidje-run-ns-tests))
              (expect 'emidje-send-test-request :to-have-been-called-with :ns `(ns "octocat.math-test")))

          (it "uses a custom inference function when I define one"
              (spy-on 'emidje-send-test-request)
              (emidje-tests-with-temp-buffer "(ns octocat.math)
(defn pow2 [x]
)"
                                             (setq-local emidje-infer-test-ns-function (lambda (ns)
                                                                                         (concat ns "-spec")))
                                             (emidje-run-ns-tests))
              (expect 'emidje-send-test-request :to-have-been-called-with :ns `(ns "octocat.math-spec")))

          (it "shows a message in the echo area by saying that tests are being run"
              (spy-on 'emidje-send-request)
              (emidje-tests-with-temp-buffer "(ns octocat.math)

(defn pow2 [x]
)"
                                             (emidje-run-ns-tests))
              (expect (emidje-tests-last-displayed-message) :to-equal "Running tests in octocat.math-test...") )

          (describe "and there are no failures and/or errors"
                    (before-each
                     (let ((response (nrepl-dict "status"
                                                 (list "done")
                                                 "results"
                                                 (nrepl-dict "octocat.math-test"
                                                             (list (nrepl-dict "context"
                                                                               (list "about math operations" "takes a number x and computes 2^x")
                                                                               "file" "/home/john-doe/projects/octocat/test/octocat/math_test.clj"
                                                                               "index" 0
                                                                               "line" 8
                                                                               "ns" "octocat.math-test"
                                                                               "type" "pass")))
                                                 "summary"
                                                 (nrepl-dict "check" 1 "error" 0 "fact" 1 "fail" 0 "finished-in" "2 seconds" "ns" 1 "pass" 1 "to-do" 0))))
                       (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function response)))
                     (emidje-tests-with-temp-buffer "(ns octocat.math)

(defn pow2 [x]
)"
                                                    (emidje-run-ns-tests)))

                    (it "calls `emidje-send-request' with the correct arguments"
                        (expect emidje-tests-op-alias :to-equal :ns)
                        (expect emidje-tests-sent-request :to-equal `(ns "octocat.math-test")))

                    (it "shows a message in the echo area by summarizing the test results"
                        (expect (emidje-tests-last-displayed-message) :to-equal "octocat.math-test: Ran 1 checks in 1 facts (2 seconds). 0 failures, 0 errors."))

                    (it "doesn't show the report buffer when
                    `emidje-always-show-test-report' is set to
                    nil"
                        (expect (emidje-tests-report-content)
                                :to-be nil)))

          (describe "and there are failures"
                    (before-each
                     (let ((response (nrepl-dict "status"
                                                 (list "done")
                                                 "results"
                                                 (nrepl-dict "octocat.math-test"
                                                             (list (nrepl-dict "actual" "9\n"
                                                                               "context"
                                                                               (list "about math operations" "takes a number x and computes 2^x")
                                                                               "expected" "8\n"
                                                                               "file" "/home/john-doe/projects/octocat/test/octocat/math_test.clj"
                                                                               "index" 0
                                                                               "line" 8
                                                                               "message" nil
                                                                               "ns" "octocat.math-test"
                                                                               "type" "fail")))
                                                 "summary"
                                                 (nrepl-dict "check" 1 "error" 0 "fact" 1 "fail" 1 "finished-in" "2 seconds" "ns" 1 "pass" 0 "to-do" 0))))
                       (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function response)))
                     (emidje-tests-with-temp-buffer "(ns octocat.math)

(defn pow2 [x]
)"
                                                    (emidje-run-ns-tests)))

                    (it "shows a message in the echo area by summarizing the test results"
                        (expect (emidje-tests-last-displayed-message) :to-equal "octocat.math-test: Ran 1 checks in 1 facts (2 seconds). 1 failures, 0 errors."))

                    (it "shows the report buffer with test results and summary"
                        (expect (emidje-tests-report-content) :to-equal
                                "Test report

** Checked namespaces (1)
octocat.math-test failed

** Test summary
Finished in 2 seconds
Ran 1 checks in 1 facts
1 failures

** Results

octocat.math-test
1 non-passing tests:

Fail in about math operations
takes a number x and computes 2^x

expected: 8

  actual: 9")))

          (describe "and there are errors"
                    (before-each
                     (let ((response (nrepl-dict "status"
                                                 (list "done")
                                                 "results"
                                                 (nrepl-dict "octocat.math-test"
                                                             (list (nrepl-dict "error" "java.lang.ArithmeticException: what?"
                                                                               "context"
                                                                               (list "about math operations" "takes a number x and computes 2^x")
                                                                               "expected" "8\n"
                                                                               "file" "/home/john-doe/projects/octocat/test/octocat/math_test.clj"
                                                                               "index" 0
                                                                               "line" 8
                                                                               "message" nil
                                                                               "ns" "octocat.math-test"
                                                                               "type" "error")))
                                                 "summary"
                                                 (nrepl-dict "check" 1 "error" 1 "fact" 1 "fail" 0 "finished-in" "2 seconds" "ns" 1 "pass" 0 "to-do" 0))))
                       (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function response)))
                     (emidje-tests-with-temp-buffer "(ns octocat.math)
(defn pow2 [x]
)"
                                                    (emidje-run-ns-tests)))

                    (it "shows a message in the echo area by summarizing the test results"
                        (expect (emidje-tests-last-displayed-message) :to-equal "octocat.math-test: Ran 1 checks in 1 facts (2 seconds). 0 failures, 1 errors."))

                    (it "shows the report buffer with test results and summary"
                        (expect (emidje-tests-report-content) :to-equal
                                "Test report

** Checked namespaces (1)
octocat.math-test failed

** Test summary
Finished in 2 seconds
Ran 1 checks in 1 facts
1 errors

** Results

octocat.math-test
1 non-passing tests:

Error in about math operations
takes a number x and computes 2^x

expected: 8

   error: java.lang.ArithmeticException: what?")))

          (describe "and there are future facts"
                    (before-each
                     (let ((response (nrepl-dict "status"
                                                 (list "done")
                                                 "results"
                                                 (nrepl-dict "octocat.math-test"
                                                             (list (nrepl-dict "error" "java.lang.ArithmeticException: what?"
                                                                               "context"
                                                                               (list "about math operations" "takes a number x and computes 2^x")
                                                                               "expected" "8\n"
                                                                               "file" "/home/john-doe/projects/octocat/test/octocat/math_test.clj"
                                                                               "index" 0
                                                                               "line" 8
                                                                               "message" nil
                                                                               "ns" "octocat.math-test"
                                                                               "type" "error")
                                                                   (nrepl-dict "context"
                                                                               (list "about math operations" "takes a number x and returns its square root")
                                                                               "file" "/home/john-doe/projects/octocat/test/octocat/math_test.clj"
                                                                               "index" 1
                                                                               "line" 13
                                                                               "ns" "octocat.math-test"
                                                                               "type" "to-do")))
                                                 "summary"
                                                 (nrepl-dict "check" 1 "error" 1 "fact" 1 "fail" 0 "finished-in" "2 seconds" "ns" 1 "pass" 0 "to-do" 0))))
                       (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function response)))
                     (emidje-tests-with-temp-buffer "(ns octocat.math)
(defn pow2 [x]
)"
                                                    (emidje-run-ns-tests)))

                    (it "shows a message in the echo area by summarizing the test results"
                        (expect (emidje-tests-last-displayed-message) :to-equal "octocat.math-test: Ran 1 checks in 1 facts (2 seconds). 0 failures, 1 errors."))

                    (it "shows the report buffer with test results and summary"
                        (expect (emidje-tests-report-content) :to-equal
                                "Test report

** Checked namespaces (1)
octocat.math-test failed

** Test summary
Finished in 2 seconds
Ran 1 checks in 1 facts
1 errors

** Results

octocat.math-test
1 non-passing tests:

Error in about math operations
takes a number x and computes 2^x

expected: 8

   error: java.lang.ArithmeticException: what?

Work To Do about math operations
takes a number x and returns its square root"))))

(describe "When I run arbitrary tests and a report is displayed"
          (let ((failed-response (nrepl-dict "status"
                                             (list "done")
                                             "results"
                                             (nrepl-dict "octocat.math-test"
                                                         (list (nrepl-dict "actual" "9\n"
                                                                           "context"
                                                                           (list "about math operations" "takes a number x and computes 2^x")
                                                                           "expected" "8\n"
                                                                           "file" "/home/john-doe/projects/octocat/test/octocat/math_test.clj"
                                                                           "index" 0
                                                                           "line" 8
                                                                           "message" nil
                                                                           "ns" "octocat.math-test"
                                                                           "type" "fail")))
                                             "summary"
                                             (nrepl-dict "check" 1 "error" 0 "fact" 1 "fail" 1 "finished-in" "2 seconds" "ns" 1 "pass" 0 "to-do" 0)) )
                (succeeded-response (nrepl-dict "status"
                                                (list "done")
                                                "results"
                                                (nrepl-dict "octocat.math-test"
                                                            (list (nrepl-dict "context"
                                                                              (list "about math operations" "takes a number x and computes 2^x")
                                                                              "file" "/home/john-doe/projects/octocat/test/octocat/math_test.clj"
                                                                              "index" 0
                                                                              "line" 8
                                                                              "ns" "octocat.math-test"
                                                                              "type" "pass")))
                                                "summary"
                                                (nrepl-dict "check" 1 "error" 0 "fact" 1 "fail" 0 "finished-in" "2 seconds" "ns" 1 "pass" 1 "to-do" 0))))

            (it "shows a shorter message in the echo area when I customize `emidje-show-full-test-summary' and tests pass"
                (emidje-tests-with-temp-buffer "(ns octocat.math)"
                                               (setq-local emidje-show-full-test-summary nil)
                                               (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function succeeded-response))
                                               (emidje-run-ns-tests))
                (expect (emidje-tests-last-displayed-message) :to-equal "All checks (1) succeeded."))

            (it "shows a shorter message in the echo area when I customize `emidje-show-full-test-summary' and tests fail"
                (emidje-tests-with-temp-buffer "(ns octocat.math)"
                                               (setq-local emidje-show-full-test-summary nil)
                                               (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function failed-response))
                                               (emidje-run-ns-tests))
                (expect (emidje-tests-last-displayed-message) :to-equal "1 checks failed, but 0 succeeded."))

            (it "kills the report buffer once I fix the failing tests and re-run them"
                (emidje-tests-with-temp-buffer "(ns octocat.math)"
                                               (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function failed-response))
                                               (emidje-run-ns-tests)
                                               (expect (get-buffer emidje-test-report-buffer) :not :to-be nil)
                                               (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function succeeded-response))
                                               (emidje-run-ns-tests)
                                               (expect (get-buffer emidje-test-report-buffer) :to-be nil)))))

(describe "When I call `emidje-run-ns-tests' with a prefix argument"
          (before-each
           (spy-on 'emidje-select-test-ns :and-call-fake (lambda (callback)
                                                           (funcall callback "octocat.colors-test")))
           (spy-on 'emidje-send-test-request)
           (emidje-run-ns-tests t))

          (it "calls `emidje-select-test-ns' and sends a test
              request with the namespace that the user has selected"
              (expect 'emidje-send-test-request :to-have-been-called-with :ns `(ns "octocat.colors-test"))))

(describe "When I call `emidje-select-test-ns'"
          :var (test-namespaces)
          (before-each
           (setq test-namespaces (list "octocat.math-test" "octocat.colors-test"))
           (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function (nrepl-dict "test-namespaces" test-namespaces
                                                                                                            "status" (list "done"))))
           (spy-on 'ido-completing-read :and-return-value "octocat.colors-test")
           (spy-on 'emidje-send-test-request)
           (emidje-select-test-ns (lambda (ns)
                                    (emidje-send-test-request :ns `(ns ,ns)))))

          (it "calls `emidje-send-request' with the expected arguments"
              (expect emidje-tests-op-alias :to-equal :test-namespaces)
              (expect emidje-tests-sent-request :to-equal nil))

          (it "calls `ido-completing-read' with the expected arguments"
              (expect 'ido-completing-read :to-have-been-called-with "Select a namespace: "
                      test-namespaces nil t))

          (it "calls the supplied callback with the namespace that has been selected by the user"
              (expect 'emidje-send-test-request :to-have-been-called-with :ns `(ns "octocat.colors-test"))))

(describe "When I open a Clojure test file, move the point to a fact and call `emidje-run-test-at-point'"
          (before-each
           (let ((response (nrepl-dict "status"
                                       (list "done")
                                       "results"
                                       (nrepl-dict "octocat.math-test"
                                                   (list (nrepl-dict "context"
                                                                     (list "about math operations" "takes a number x and computes 2^x")
                                                                     "file" "/home/john-doe/projects/octocat/test/octocat/math_test.clj"
                                                                     "index" 0
                                                                     "line" 8
                                                                     "ns" "octocat.math-test"
                                                                     "type" "pass")))
                                       "summary"
                                       (nrepl-dict "check" 1 "error" 0 "fact" 1 "fail" 0 "finished-in" "2 milliseconds" "ns" 1 "pass" 1 "to-do" 0))))
             (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function response)))
           (emidje-tests-with-temp-buffer "(ns octocat.math-test)

(fact \"takes a number x and returns 2^x\"
(math/pow2 3) => 8)"
                                          (forward-line 2)
                                          (emidje-run-test-at-point)))

          (it "shows a message in the echo area by saying that tests are being run"
              (expect (emidje-tests-last-displayed-message 2) :to-equal
                      "Running test \"takes a number x and returns 2^x\" in octocat.math-test..."))

          (it "shows the correct message even if the fact in question has no description"
              (emidje-tests-with-temp-buffer "(ns octocat.math-test)

(fact
(math/pow2 3) => 8)"
               (forward-line 2)
               (emidje-run-test-at-point))
              (expect (emidje-tests-last-displayed-message 2) :to-equal
                      "Running test in octocat.math-test..."))

          (it "shows a message in the echo area by summarizing the test results"
              (expect (emidje-tests-last-displayed-message) :to-equal
                      "Ran 1 checks in 1 facts (2 milliseconds). 0 failures, 0 errors."))

          (it "calls `emidje-send-request' with the correct arguments"
              (expect emidje-tests-op-alias :to-equal :test-at-point)
              (expect emidje-tests-sent-request :to-have-same-items-as `(ns "octocat.math-test"
                                                                            source "(fact \"takes a number x and returns 2^x\"
(math/pow2 3) => 8)"
                                                                            line 3))))

(describe "When I call `emidje-run-all-tests'"
          (before-each
           (let ((response (nrepl-dict "status"
                                       (list "done")
                                       "results"
                                       (nrepl-dict "octocat.math-test"
                                                   (list (nrepl-dict "actual" "9\n"
                                                                     "context"
                                                                     (list "about math operations" "takes a number x and computes 2^x")
                                                                     "expected" "8\n"
                                                                     "file" "/home/john-doe/projects/octocat/test/octocat/math_test.clj"
                                                                     "index" 0
                                                                     "line" 8
                                                                     "message" nil
                                                                     "ns" "octocat.math-test"
                                                                     "type" "fail"))
                                                   "octocat.colors-test"
                                                   (list (nrepl-dict "actual" ":orange\n"
                                                                     "context"
                                                                     (list "about mixing colors" "blue + yellow produces green")
                                                                     "expected" ":green\n"
                                                                     "file" "/home/john-doe/projects/octocat/test/octocat/colors_test.clj"
                                                                     "index" 0
                                                                     "line" 8
                                                                     "message" (list "This is a message")
                                                                     "ns" "octocat.colors-test"
                                                                     "type" "fail")))
                                       "summary"
                                       (nrepl-dict "check" 2 "error" 0 "fact" 2 "fail" 2 "finished-in" "3 seconds" "ns" 2 "pass" 0 "to-do" 0))))
             (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function response)))
           (spy-on 'emidje-select-test-path)
           (emidje-run-all-tests))

          (it "calls `emidje-send-request' with the correct arguments"
              (expect emidje-tests-op-alias :to-equal :project)
              (expect emidje-tests-sent-request :to-be nil))

          (it "shows a message in the echo area by saying that tests are being run"
              (expect (emidje-tests-last-displayed-message 2)
                      :to-equal "Running tests in all project namespaces..."))

          (it "shows a message in the echo area by displaying the test summary"
              (expect (emidje-tests-last-displayed-message)
                      :to-equal "Ran 2 checks in 2 facts (3 seconds). 2 failures, 0 errors."))

          (it "shows a report buffer by displaying test results and summary"
              (expect (emidje-tests-report-content) :to-equal
                      "Test report

** Checked namespaces (2)
octocat.colors-test failed
octocat.math-test   failed

** Test summary
Finished in 3 seconds
Ran 2 checks in 2 facts
2 failures

** Results

octocat.math-test
1 non-passing tests:

Fail in about math operations
takes a number x and computes 2^x

expected: 8

  actual: 9\t\s\s

octocat.colors-test
1 non-passing tests:

Fail in about mixing colors
blue + yellow produces green

expected: :green\t\s\s

  actual: :orange\t\s\s

Checker said about the reason: This is a message")))

(describe "When I call `emidje-select-test-path'"

          (describe "and the project has more than one test path set"
                    (before-each
                     (spy-on 'emidje-send-request :and-return-value (nrepl-dict
                                                                     "status" (list "done")
                                                                     "test-paths" (list "integration" "test")))
                     (spy-on 'ido-completing-read :and-return-value "test"))

                    (it "calls `emidje-send-request' with the expected arguments"
                        (emidje-select-test-path nil nil)
                        (expect 'emidje-send-request :to-have-been-called-with :test-paths))

                    (it "calls `ido-completing-read' with the expected arguments"
                        (emidje-select-test-path nil nil)
                        (expect 'ido-completing-read :to-have-been-called-with "Select a test path: " (list "integration" "test")
                                nil t nil))

                    (it "shows the last value set as a completion tip when one is supplied"
                        (emidje-select-test-path nil `("test"))
                        (expect 'ido-completing-read :to-have-been-called-with "Select a test path: " (list "integration" "test")
                                nil t "test"))

                    (it "returns the selected test path inside a list"
                        (expect               (emidje-select-test-path nil nil) :to-equal `("test")))))

(describe "When I call `emidje-read-list-from-popup-option'"
          (before-each
           (spy-on 'read-from-minibuffer :and-return-value "^integration service-test"))

          (it "reads values from the minibuffer and returns them
              as a list"
              (expect (emidje-read-list-from-popup-option "ns-exclusions=" nil) :to-equal `("^integration" "service-test"))
              (expect 'read-from-minibuffer :to-have-been-called-with "ns-exclusions=" nil))

          (it "shows the last entered list of values as a
              possible input"
              (expect (emidje-read-list-from-popup-option "ns-exclusions=" `("^integration" "datomic-client-test")) :to-equal `("^integration" "service-test"))
              (expect 'read-from-minibuffer :to-have-been-called-with "ns-exclusions=" "^integration datomic-client-test")))

(describe "When I call `emidje-parse-popup-args'"

          (it "parses Magit popup arguments with switches and/or
          options, and turns them into a valid request to be sent
          to nREPL"
              (expect (emidje-parse-popup-args `("coverage?"
                                                 "ns-exclusions=(too-slow-test colors-test)"
                                                 "ns-inclusions=(^adapters?)"
                                                 "top-slowest-tests=5"))
                      :to-equal `(coverage? "true"
                                            ns-exclusions ("too-slow-test" "colors-test")
                                            ns-inclusions ("^adapters?")
                                            top-slowest-tests 5))))

(describe "When I call `emidje-run-all-tests' interactively with
arguments"
          (before-each
           (let ((response (nrepl-dict "status"
                                       (list "done")
                                       "results"
                                       (nrepl-dict "octocat.math-test"
                                                   (list (nrepl-dict "actual" "9\n"
                                                                     "context"
                                                                     (list "about math operations" "takes a number x and computes 2^x")
                                                                     "expected" "8\n"
                                                                     "file" "/home/john-doe/projects/octocat/test/octocat/math_test.clj"
                                                                     "index" 0
                                                                     "line" 8
                                                                     "message" nil
                                                                     "ns" "octocat.math-test"
                                                                     "type" "fail"))
                                                   "octocat.colors"
                                                   (list (nrepl-dict "actual" ":orange\n"
                                                                     "context"
                                                                     (list "about mixing colors" "blue + yellow produces green")
                                                                     "expected" ":green\n"
                                                                     "file" "/home/john-doe/projects/octocat/test/octocat/colors_test.clj"
                                                                     "index" 0
                                                                     "line" 8
                                                                     "message" (list "This is a message")
                                                                     "ns" "octocat.colors-test"
                                                                     "type" "fail")))
                                       "summary"
                                       (nrepl-dict "check" 2 "error" 0 "fact" 2 "fail" 2 "finished-in" "3 seconds" "ns" 2 "pass" 0 "to-do" 0))))
             (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function response)))
           (setq emidje-run-all-tests-arguments `("test-paths=(test)" "ns-exclusions=(too-slow-test)"))
           (call-interactively #'emidje-run-all-tests))

          (it "calls `emidje-send-request' with the correct arguments"
              (expect emidje-tests-op-alias :to-equal :project)
              (expect emidje-tests-sent-request :to-equal `(test-paths ("test") ns-exclusions ("too-slow-test"))))

          (it "shows a message in the echo area by saying that tests are being run"
              (expect (emidje-tests-last-displayed-message 2)
                      :to-equal "Running tests in the test directory...")))

(describe "When I call `emidje-re-run-non-passing-tests'"
          (before-each
           (let ((response (nrepl-dict "status"
                                       (list "done")
                                       "results"
                                       (nrepl-dict "octocat.math-test"
                                                   (list (nrepl-dict "actual" "9\n"
                                                                     "context"
                                                                     (list "about math operations" "takes a number x and computes 2^x")
                                                                     "expected" "8\n"
                                                                     "file" "/home/john-doe/projects/octocat/test/octocat/math_test.clj"
                                                                     "index" 0
                                                                     "line" 8
                                                                     "message" nil
                                                                     "ns" "octocat.math-test"
                                                                     "type" "fail"))
                                                   "octocat.colors-test"
                                                   (list (nrepl-dict "actual" ":orange\n"
                                                                     "context"
                                                                     (list "about mixing colors" "blue + yellow produces green")
                                                                     "expected" ":green\n"
                                                                     "file" "/home/john-doe/projects/octocat/test/octocat/colors_test.clj"
                                                                     "index" 0
                                                                     "line" 8
                                                                     "message" (list "This is a message")
                                                                     "ns" "octocat.colors-test"
                                                                     "type" "fail")))
                                       "summary"
                                       (nrepl-dict "check" 2 "error" 0 "fact" 2 "fail" 2 "finished-in" "3 seconds" "ns" 2 "pass" 0 "to-do" 0))))
             (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function response)))
           (emidje-re-run-non-passing-tests))

          (it "calls `emidje-send-request' with the correct arguments"
              (expect emidje-tests-op-alias :to-equal :retest)
              (expect emidje-tests-sent-request :to-be nil))

          (it "shows a message in the echo area by saying that tests are being run"
              (expect (emidje-tests-last-displayed-message 2)
                      :to-equal "Re-running non-passing tests..."))

          (it "shows a message in the echo area by displaying the test summary"
              (expect (emidje-tests-last-displayed-message)
                      :to-equal "Ran 2 checks in 2 facts (3 seconds). 2 failures, 0 errors."))

          (it "shows a report buffer by displaying test results and summary"
              (expect (emidje-tests-report-content) :to-equal
                      "Test report

** Checked namespaces (2)
octocat.colors-test failed
octocat.math-test   failed

** Test summary
Finished in 3 seconds
Ran 2 checks in 2 facts
2 failures

** Results

octocat.math-test
1 non-passing tests:

Fail in about math operations
takes a number x and computes 2^x

expected: 8

  actual: 9\t\s\s

octocat.colors-test
1 non-passing tests:

Fail in about mixing colors
blue + yellow produces green

expected: :green\t\s\s

  actual: :orange\t\s\s

Checker said about the reason: This is a message")))
