;;; emidje-collect-profiling-information-tests --- Tests for Emidje -*- lexical-binding: t -*-

;; Copyright © 2018 Nubank

;; This file is not part of GNU Emacs

;;; Commentary:

;; Tests for Emidje.

;;; Code:

(require 'buttercup)
(require 'emidje)
(require 'emidje-test-helpers)

(defconst emidje-response-with-failures-and-more-than-one-slowest-test (nrepl-dict "status"
                                                                                   (list "done")
                                                                                   "profile"
                                                                                   (nrepl-dict "average" "12 milliseconds" "namespaces"
                                                                                               (list (nrepl-dict "average" "1 millisecond" "ns" "octocat.arithmetic-test" "number-of-tests" 4 "percent-of-total-time" "6.56%" "total-time" "4 milliseconds")
                                                                                                     (nrepl-dict "average" "1 millisecond" "ns" "octocat.side-effects-test" "number-of-tests" 1 "percent-of-total-time" "1.64%" "total-time" "1 millisecond"))
                                                                                               "number-of-tests" 5 "top-slowest-tests"
                                                                                               (nrepl-dict "percent-of-total-time" "8.2%" "tests"
                                                                                                           (list (nrepl-dict "context"
                                                                                                                             (list "about arithmetic operations" "(fact (* 2 5) => 10 :position (pointer.core/line-number-known 6))")
                                                                                                                             "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "line" 6 "total-time" "1 millisecond")
                                                                                                                 (nrepl-dict "context"
                                                                                                                             (list "about arithmetic operations" "this is a crazy arithmetic")
                                                                                                                             "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "line" 9 "total-time" "1 millisecond")
                                                                                                                 (nrepl-dict "context"
                                                                                                                             (list "about arithmetic operations" "two assertions in the same fact; the former is correct while the later is wrong")
                                                                                                                             "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "line" 13 "total-time" "1 millisecond")
                                                                                                                 (nrepl-dict "context"
                                                                                                                             (list "about arithmetic operations" "this will throw an unexpected exception")
                                                                                                                             "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "line" 16 "total-time" "1 millisecond")
                                                                                                                 (nrepl-dict "context"
                                                                                                                             (list "about write-hello-world" "writes a greeting file")
                                                                                                                             "file" "/home/john-doe/projects/octocat/test/octocat/side_effects_test.clj" "line" 18 "total-time" "1 millisecond"))
                                                                                                           "total-time" "5 milliseconds")
                                                                                               "total-time" "61 milliseconds")
                                                                                   "results"
                                                                                   (nrepl-dict "octocat.arithmetic-test"
                                                                                               (list (nrepl-dict "context"
                                                                                                                 (list "about arithmetic operations" "(fact (* 2 5) => 10 :position (pointer.core/line-number-known 6))")
                                                                                                                 "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "id" "166599b48516e9f70c4fd19758f7daee39e7b014" "index" 0 "line" 6 "ns" "octocat.arithmetic-test" "type" "pass")
                                                                                                     (nrepl-dict "actual" "5\n" "context"
                                                                                                                 (list "about arithmetic operations" "this is a crazy arithmetic")
                                                                                                                 "expected" "6\n" "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "index" 1 "line" 9 "message" nil "ns" "octocat.arithmetic-test" "type" "fail")
                                                                                                     (nrepl-dict "context"
                                                                                                                 (list "about arithmetic operations" "two assertions in the same fact; the former is correct while the later is wrong")
                                                                                                                 "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "index" 2 "line" 11 "ns" "octocat.arithmetic-test" "type" "pass")
                                                                                                     (nrepl-dict "actual" "2\n" "context"
                                                                                                                 (list "about arithmetic operations" "two assertions in the same fact; the former is correct while the later is wrong")
                                                                                                                 "expected" "3\n" "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "index" 3 "line" 13 "message" nil "ns" "octocat.arithmetic-test" "type" "fail")
                                                                                                     (nrepl-dict "context"
                                                                                                                 (list "about arithmetic operations" "this will throw an unexpected exception")
                                                                                                                 "error" "java.lang.ArithmeticException: Divide by zero" "expected" "0\n" "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "index" 4 "line" 16 "ns" "octocat.arithmetic-test" "type" "error"))
                                                                                               "octocat.side-effects-test"
                                                                                               (list (nrepl-dict "context"
                                                                                                                 (list "about write-hello-world" "writes a greeting file")
                                                                                                                 "file" "/home/john-doe/projects/octocat/test/octocat/side_effects_test.clj" "index" 0 "line" 18 "ns" "octocat.side-effects-test" "type" "pass")))
                                                                                   "summary"
                                                                                   (nrepl-dict "check" 6 "error" 1 "fact" 5 "fail" 2 "finished-in" "61 milliseconds" "ns" 2 "pass" 3 "to-do" 0)))

(defconst emidje-response-with-no-failures-and-more-than-one-slowest-test (nrepl-dict "status"
                                                                                   (list "done")
                                                                                   "profile"
                                                                                   (nrepl-dict "average" "12 milliseconds" "namespaces"
                                                                                               (list (nrepl-dict "average" "1 millisecond" "ns" "octocat.arithmetic-test" "number-of-tests" 4 "percent-of-total-time" "6.56%" "total-time" "4 milliseconds")
                                                                                                     (nrepl-dict "average" "1 millisecond" "ns" "octocat.side-effects-test" "number-of-tests" 1 "percent-of-total-time" "1.64%" "total-time" "1 millisecond"))
                                                                                               "number-of-tests" 5 "top-slowest-tests"
                                                                                               (nrepl-dict "percent-of-total-time" "8.2%" "tests"
                                                                                                           (list (nrepl-dict "context"
                                                                                                                             (list "about arithmetic operations" "(fact (* 2 5) => 10 :position (pointer.core/line-number-known 6))")
                                                                                                                             "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "line" 6 "total-time" "1 millisecond")
                                                                                                                 (nrepl-dict "context"
                                                                                                                             (list "about arithmetic operations" "this is a crazy arithmetic")
                                                                                                                             "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "line" 9 "total-time" "1 millisecond")
                                                                                                                 (nrepl-dict "context"
                                                                                                                             (list "about arithmetic operations" "two assertions in the same fact; the former is correct while the later is wrong")
                                                                                                                             "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "line" 13 "total-time" "1 millisecond")
                                                                                                                 (nrepl-dict "context"
                                                                                                                             (list "about arithmetic operations" "this will throw an unexpected exception")
                                                                                                                             "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "line" 16 "total-time" "1 millisecond")
                                                                                                                 (nrepl-dict "context"
                                                                                                                             (list "about write-hello-world" "writes a greeting file")
                                                                                                                             "file" "/home/john-doe/projects/octocat/test/octocat/side_effects_test.clj" "line" 18 "total-time" "1 millisecond"))
                                                                                                           "total-time" "5 milliseconds")
                                                                                               "total-time" "61 milliseconds")
                                                                                   "results"
                                                                                   (nrepl-dict "octocat.arithmetic-test"
                                                                                               (list (nrepl-dict "context"
                                                                                                                 (list "about arithmetic operations" "(fact (* 2 5) => 10 :position (pointer.core/line-number-known 6))")
                                                                                                                 "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "id" "166599b48516e9f70c4fd19758f7daee39e7b014" "index" 0 "line" 6 "ns" "octocat.arithmetic-test" "type" "pass")
                                                                                                     (nrepl-dict "context"
                                                                                                                 (list "about arithmetic operations" "this is a crazy arithmetic")
                                                                                                                 "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "index" 1 "line" 9 "message" nil "ns" "octocat.arithmetic-test" "type" "pass")
                                                                                                     (nrepl-dict "context"
                                                                                                                 (list "about arithmetic operations" "two assertions in the same fact; the former is correct while the later is wrong")
                                                                                                                 "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "index" 2 "line" 11 "ns" "octocat.arithmetic-test" "type" "pass")
                                                                                                     (nrepl-dict "context"
                                                                                                                 (list "about arithmetic operations" "two assertions in the same fact; the former is correct while the later is wrong")
                                                                                                                 "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "index" 3 "line" 13 "message" nil "ns" "octocat.arithmetic-test" "type" "pass")
                                                                                                     (nrepl-dict "context"
                                                                                                                 (list "about arithmetic operations" "this will throw an unexpected exception")
                                                                                                                 "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "index" 4 "line" 16 "ns" "octocat.arithmetic-test" "type" "pass"))
                                                                                               "octocat.side-effects-test"
                                                                                               (list (nrepl-dict "context"
                                                                                                                 (list "about write-hello-world" "writes a greeting file")
                                                                                                                 "file" "/home/john-doe/projects/octocat/test/octocat/side_effects_test.clj" "index" 0 "line" 18 "ns" "octocat.side-effects-test" "type" "pass")))
                                                                                   "summary"
                                                                                   (nrepl-dict "check" 6 "error" 0 "fact" 5 "fail" 0 "finished-in" "61 milliseconds" "ns" 2 "pass" 6 "to-do" 0)))

(defconst emidje-response-with-no-failures-and-the-slowest-test (nrepl-dict "status"
                                                                                 (list "done")
                                                                                 "profile"
                                                                                 (nrepl-dict "average" "12 milliseconds" "namespaces"
                                                                                             (list (nrepl-dict "average" "1 millisecond" "ns" "octocat.arithmetic-test" "number-of-tests" 4 "percent-of-total-time" "6.56%" "total-time" "4 milliseconds")
                                                                                                   (nrepl-dict "average" "1 millisecond" "ns" "octocat.side-effects-test" "number-of-tests" 1 "percent-of-total-time" "1.64%" "total-time" "1 millisecond"))
                                                                                             "number-of-tests" 5 "top-slowest-tests"
                                                                                             (nrepl-dict "percent-of-total-time" "8.2%" "tests"
                                                                                                         (list (nrepl-dict "context"
                                                                                                                           (list "about arithmetic operations" "(fact (* 2 5) => 10 :position (pointer.core/line-number-known 6))")
                                                                                                                           "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "line" 6 "total-time" "1 millisecond"))
                                                                                                         "total-time" "5 milliseconds")
                                                                                             "total-time" "61 milliseconds")
                                                                                 "results"
                                                                                 (nrepl-dict "octocat.arithmetic-test"
                                                                                             (list (nrepl-dict "context"
                                                                                                               (list "about arithmetic operations" "(fact (* 2 5) => 10 :position (pointer.core/line-number-known 6))")
                                                                                                               "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "id" "166599b48516e9f70c4fd19758f7daee39e7b014" "index" 0 "line" 6 "ns" "octocat.arithmetic-test" "type" "pass")
                                                                                                   (nrepl-dict "context"
                                                                                                               (list "about arithmetic operations" "this is a crazy arithmetic")
                                                                                                               "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "index" 1 "line" 9 "message" nil "ns" "octocat.arithmetic-test" "type" "pass")
                                                                                                   (nrepl-dict "context"
                                                                                                               (list "about arithmetic operations" "two assertions in the same fact; the former is correct while the later is wrong")
                                                                                                               "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "index" 2 "line" 11 "ns" "octocat.arithmetic-test" "type" "pass")
                                                                                                   (nrepl-dict "context"
                                                                                                               (list "about arithmetic operations" "two assertions in the same fact; the former is correct while the later is wrong")
                                                                                                               "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "index" 3 "line" 13 "message" nil "ns" "octocat.arithmetic-test" "type" "pass")
                                                                                                   (nrepl-dict "context"
                                                                                                               (list "about arithmetic operations" "this will throw an unexpected exception")
                                                                                                               "file" "/home/john-doe/projects/octocat/test/octocat/arithmetic_test.clj" "index" 4 "line" 16 "ns" "octocat.arithmetic-test" "type" "pass"))
                                                                                             "octocat.side-effects-test"
                                                                                             (list (nrepl-dict "context"
                                                                                                               (list "about write-hello-world" "writes a greeting file")
                                                                                                               "file" "/home/john-doe/projects/octocat/test/octocat/side_effects_test.clj" "index" 0 "line" 18 "ns" "octocat.side-effects-test" "type" "pass")))
                                                                                 "summary"
                                                                                 (nrepl-dict "check" 6 "error" 0 "fact" 5 "fail" 0 "finished-in" "61 milliseconds" "ns" 2 "pass" 6 "to-do" 0)))

(describe "When the nREPL middleware responds with profiling information"
          (before-each
           (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function emidje-response-with-failures-and-more-than-one-slowest-test))
           (call-interactively #'emidje-run-all-tests `("profile?")))

          (it "shows the report buffer by displaying profiling information"
              (expect (emidje-tests-report-content) :to-equal
                      "Test report

** Checked namespaces (2)
octocat.arithmetic-test   failed
octocat.side-effects-test passed

** Test summary
Finished in 61 milliseconds
Ran 6 checks in 5 facts
2 failures
1 errors

** Profile
12 milliseconds average (61 milliseconds / 5 tests)
Top 5 slowest tests (5 milliseconds, 8.2% of total time):
about arithmetic operations
(fact (* 2 5) => 10 :position (pointer.core/line-number-known 6))
1 millisecond
about arithmetic operations
this is a crazy arithmetic
1 millisecond
about arithmetic operations
two assertions in the same fact; the former is correct while the later is wrong
1 millisecond
about arithmetic operations
this will throw an unexpected exception
1 millisecond
about write-hello-world
writes a greeting file
1 millisecond

*** Namespaces (slowest first)
octocat.arithmetic-test: 1 millisecond average (4 milliseconds / 4 tests)
6.56% of total time
octocat.side-effects-test: 1 millisecond average (1 millisecond / 1 tests)
1.64% of total time

** Results

octocat.arithmetic-test
3 non-passing tests:

Fail in about arithmetic operations
this is a crazy arithmetic

expected: 6

  actual: 5\t\s\s

Fail in about arithmetic operations
two assertions in the same fact; the former is correct while the later is wrong

expected: 3\t\s\s

  actual: 2\t\s\s

Error in about arithmetic operations
this will throw an unexpected exception

expected: 0\t\s\s

   error: java.lang.ArithmeticException: Divide by zero")))

(describe "When the nREPL middleware responds with profiling information, but there are no failures"
          (before-each
           (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function emidje-response-with-no-failures-and-more-than-one-slowest-test))
           (setq emidje-run-all-tests-arguments `("profile?"))
           (call-interactively #'emidje-run-all-tests))

          (it "always shows the report buffer, since the user wants to view profiling information"
              (expect (emidje-tests-report-content) :to-equal
                      "Test report

** Checked namespaces (2)
octocat.arithmetic-test   passed
octocat.side-effects-test passed

** Test summary
Finished in 61 milliseconds
Ran 6 checks in 5 facts
6 passed

** Profile
12 milliseconds average (61 milliseconds / 5 tests)
Top 5 slowest tests (5 milliseconds, 8.2% of total time):
about arithmetic operations
(fact (* 2 5) => 10 :position (pointer.core/line-number-known 6))
1 millisecond
about arithmetic operations
this is a crazy arithmetic
1 millisecond
about arithmetic operations
two assertions in the same fact; the former is correct while the later is wrong
1 millisecond
about arithmetic operations
this will throw an unexpected exception
1 millisecond
about write-hello-world
writes a greeting file
1 millisecond

*** Namespaces (slowest first)
octocat.arithmetic-test: 1 millisecond average (4 milliseconds / 4 tests)
6.56% of total time
octocat.side-effects-test: 1 millisecond average (1 millisecond / 1 tests)
1.64% of total time")))

(describe "When the nREPL middleware responds with profiling information and the user wants to se only the slowest test"
          (before-each
           (spy-on 'emidje-send-request :and-call-fake (emidje-tests-fake-send-request-function emidje-response-with-no-failures-and-the-slowest-test))
           (setq emidje-run-all-tests-arguments `("profile?"))
           (call-interactively #'emidje-run-all-tests))

          (it "shows the report buffer by displaying profiling information"
              (expect (emidje-tests-report-content) :to-equal
                      "Test report

** Checked namespaces (2)
octocat.arithmetic-test   passed
octocat.side-effects-test passed

** Test summary
Finished in 61 milliseconds
Ran 6 checks in 5 facts
6 passed

** Profile
12 milliseconds average (61 milliseconds / 5 tests)
Slowest test (5 milliseconds, 8.2% of total time):
about arithmetic operations
(fact (* 2 5) => 10 :position (pointer.core/line-number-known 6))

*** Namespaces (slowest first)
octocat.arithmetic-test: 1 millisecond average (4 milliseconds / 4 tests)
6.56% of total time
octocat.side-effects-test: 1 millisecond average (1 millisecond / 1 tests)
1.64% of total time")))
