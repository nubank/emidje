;;; emidje-tests.el --- Tests for Emidje -*- lexical-binding: t -*-

;; Copyright © 2018 Nubank

;; This file is not part of GNU Emacs

;;; Commentary:

;; This file is part of Emidje.

;;; Code:

(require 'buttercup)
(require 'emidje)
(nrepl-dict "status"
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
            (nrepl-dict "check" 1 "error" 0 "fact" 1 "fail" 1 "ns" 1 "pass" 0 "to-do" 0))

(describe "When I call `emidje-run-all-tests'"

          (it "sends a request to the nREPL middleware to run all tests (facts) defined in the project"
              (spy-on 'emidje-send-test-request)
              (emidje-run-all-tests)
              (expect 'emidje-send-test-request :to-have-been-called-with :project)))

(describe "When I open a Clojure source file and call `emidje-run-ns-tests'"

          (it "sends a test request to nREPL middleware to run all facts in the corresponding test namespace"
              (spy-on 'cider-current-ns :and-return-value "octocat.math")
              (spy-on 'emidje-send-test-request)
              (emidje-run-ns-tests)
              (expect 'emidje-send-test-request :to-have-been-called-with :ns `(ns "octocat.math-test"))))

(describe "When I open a Clojure test file and call `emidje-run-ns-tests'"

          (it "sends a test request to nREPL middleware to run all facts in the corresponding test namespace"
              (spy-on 'cider-current-ns :and-return-value "octocat.math-test")
              (spy-on 'emidje-send-test-request)
              (emidje-run-ns-tests)
              (expect 'emidje-send-test-request :to-have-been-called-with :ns `(ns "octocat.math-test"))))

(describe "When I open a Clojure test file, move point to a fact and call `emidje-run-test-at-point'"
          (it "sends a request to the nREPL middleware to run the fact in question"
              (spy-on 'emidje-send-test-request)
              (with-temp-buffer
                (insert "(ns octocat.math-test)\n\n")
                (insert "(fact
(math/pow2 3) => 8)")
                (goto-char (point-min))
                (forward-sexp 2)
                (emidje-run-test-at-point))
              (expect 'emidje-send-test-request :to-have-been-called-with :test-at-point `(ns "octocat.math-test"
                                                                                              source "(fact
(math/pow2 3) => 8)"
                                                                                              line 4))))

(describe "When I call `emidje-re-run-non-passing-tests'"

          (it "sends a test request to nREPL middleware to re-run tests (facts) that didn't pass in the last execution"
              (spy-on 'emidje-send-test-request)
              (emidje-re-run-non-passing-tests)
              (expect 'emidje-send-test-request :to-have-been-called-with :retest)))
