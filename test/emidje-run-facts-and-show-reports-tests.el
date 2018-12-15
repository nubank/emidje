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
                                                 (nrepl-dict "check" 1 "error" 0 "fact" 1 "fail" 0 "ns" 1 "pass" 1 "to-do" 0))))
                       (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function response)))
                     (emidje-tests-with-temp-buffer "(ns octocat.math)

(defn pow2 [x]
)"
                                                    (emidje-run-ns-tests)))

                    (it "calls `emidje-send-request' with the correct arguments"
                        (expect emidje-tests-op-alias :to-equal :ns)
                        (expect emidje-tests-sent-request :to-equal `(ns "octocat.math-test")))

                    (it "shows a message in the echo area by summarizing the test results"
                        (expect (emidje-tests-last-displayed-message) :to-equal "octocat.math-test: Ran 1 checks in 1 facts. 0 failures, 0 errors."))

                    (it "doesn't show the report buffer"
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
                                                 (nrepl-dict "check" 1 "error" 0 "fact" 1 "fail" 1 "ns" 1 "pass" 0 "to-do" 0))))
                       (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function response)))
                     (emidje-tests-with-temp-buffer "(ns octocat.math)

(defn pow2 [x]
)"
                                                    (emidje-run-ns-tests)))

                    (it "shows a message in the echo area by summarizing the test results"
                        (expect (emidje-tests-last-displayed-message) :to-equal "octocat.math-test: Ran 1 checks in 1 facts. 1 failures, 0 errors."))

                    (it "shows the report buffer with test results and summary"
                        (expect (emidje-tests-report-content) :to-equal
                                "Test Summary

octocat.math-test

Checked 1 namespaces
Ran 1 checks in 1 facts
1 failures

Results

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
                                                 (nrepl-dict "check" 1 "error" 1 "fact" 1 "fail" 0 "ns" 1 "pass" 0 "to-do" 0))))
                       (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function response)))
                     (emidje-tests-with-temp-buffer "(ns octocat.math)
(defn pow2 [x]
)"
                                                    (emidje-run-ns-tests)))

                    (it "shows a message in the echo area by summarizing the test results"
                        (expect (emidje-tests-last-displayed-message) :to-equal "octocat.math-test: Ran 1 checks in 1 facts. 0 failures, 1 errors."))

                    (it "shows the report buffer with test results and summary"
                        (expect (emidje-tests-report-content) :to-equal
                                "Test Summary

octocat.math-test

Checked 1 namespaces
Ran 1 checks in 1 facts
1 errors

Results

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
                                                 (nrepl-dict "check" 1 "error" 1 "fact" 1 "fail" 0 "ns" 1 "pass" 0 "to-do" 0))))
                       (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function response)))
                     (emidje-tests-with-temp-buffer "(ns octocat.math)
(defn pow2 [x]
)"
                                                    (emidje-run-ns-tests)))

                    (it "shows a message in the echo area by summarizing the test results"
                        (expect (emidje-tests-last-displayed-message) :to-equal "octocat.math-test: Ran 1 checks in 1 facts. 0 failures, 1 errors."))

                    (it "shows the report buffer with test results and summary"
                        (expect (emidje-tests-report-content) :to-equal
                                "Test Summary

octocat.math-test

Checked 1 namespaces
Ran 1 checks in 1 facts
1 errors

Results

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
                                             (nrepl-dict "check" 1 "error" 0 "fact" 1 "fail" 1 "ns" 1 "pass" 0 "to-do" 0)) )
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
                                                (nrepl-dict "check" 1 "error" 0 "fact" 1 "fail" 0 "ns" 1 "pass" 1 "to-do" 0))))

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
                                       (nrepl-dict "check" 1 "error" 0 "fact" 1 "fail" 0 "ns" 1 "pass" 1 "to-do" 0))))
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
                      "Ran 1 checks in 1 facts. 0 failures, 0 errors."))

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
                                       (nrepl-dict "check" 2 "error" 0 "fact" 2 "fail" 2 "ns" 2 "pass" 0 "to-do" 0))))
             (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function response)))
           (spy-on 'emidje-select-test-path)
           (emidje-run-all-tests))

          (it "doesn't call `emidje-select-test-path'"
              (expect 'emidje-select-test-path ) :not :to-have-been-called)

          (it "calls `emidje-send-request' with the correct arguments"
              (expect emidje-tests-op-alias :to-equal :project)
              (expect emidje-tests-sent-request :to-be nil))

          (it "shows a message in the echo area by saying that tests are being run"
              (expect (emidje-tests-last-displayed-message 2)
                      :to-equal "Running tests in all project namespaces..."))

          (it "shows a message in the echo area by displaying the test summary"
              (expect (emidje-tests-last-displayed-message)
                      :to-equal "Ran 2 checks in 2 facts. 2 failures, 0 errors."))

          (it "shows a report buffer by displaying test results and summary"
              (expect (emidje-tests-report-content) :to-equal
                      "Test Summary

octocat.math-test

octocat.colors

Checked 2 namespaces
Ran 2 checks in 2 facts
2 failures

Results

octocat.math-test
1 non-passing tests:

Fail in about math operations
takes a number x and computes 2^x

expected: 8

  actual: 9\t\s\s

octocat.colors
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
                     (spy-on 'ido-completing-read :and-return-value "test")
                     (emidje-select-test-path))

                    (it "calls `emidje-send-request' with the expected arguments"
                        (expect 'emidje-send-request :to-have-been-called-with :test-paths))

                    (it "calls `ido-completing-read' with the expected arguments"
                        (expect 'ido-completing-read :to-have-been-called-with "Select a test path:" (list "integration" "test"))))

          )

(describe "When I call `emidje-run-all-tests' with a prefix argument"
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
                                       (nrepl-dict "check" 2 "error" 0 "fact" 2 "fail" 2 "ns" 2 "pass" 0 "to-do" 0))))
             (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function response))
             (spy-on 'emidje-select-test-path :and-return-value "test"))
           (emidje-run-all-tests t))

          (it "calls `emidje-send-request' with the correct arguments"
              (expect emidje-tests-op-alias :to-equal :project)
              (expect emidje-tests-sent-request :to-equal `(test-paths ("test"))))

          (it "shows a message in the echo area by saying that tests are being run"
              (expect (emidje-tests-last-displayed-message 2)
                      :to-equal "Running tests in the test folder...")))

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
                                       (nrepl-dict "check" 2 "error" 0 "fact" 2 "fail" 2 "ns" 2 "pass" 0 "to-do" 0))))
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
                      :to-equal "Ran 2 checks in 2 facts. 2 failures, 0 errors."))

          (it "shows a report buffer by displaying test results and summary"
              (expect (emidje-tests-report-content) :to-equal
                      "Test Summary

octocat.math-test

octocat.colors

Checked 2 namespaces
Ran 2 checks in 2 facts
2 failures

Results

octocat.math-test
1 non-passing tests:

Fail in about math operations
takes a number x and computes 2^x

expected: 8

  actual: 9\t\s\s

octocat.colors
1 non-passing tests:

Fail in about mixing colors
blue + yellow produces green

expected: :green\t\s\s

  actual: :orange\t\s\s

Checker said about the reason: This is a message")))
