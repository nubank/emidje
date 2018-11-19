;;; emidje-features-tests.el --- Tests for Emidje -*- lexical-binding: t -*-

;; Copyright © 2018 Nubank

;; This file is not part of GNU Emacs

;;; Commentary:

;; This file is part of Emidje.

;;; Code:

(require 'buttercup)
(require 'emidje)

(defun emidje-specs-read-test-report ()
  (sleep-for 5)
  (with-current-buffer (get-buffer emidje-test-report-buffer)
    (buffer-string)))

(describe "When I visit a Clojure source file"

          (describe "and I call `emidje-run-ns-tests'"
                    :var (response report)

                    (setq-local response (nrepl-dict "status"
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
                                                     (nrepl-dict "check" 1 "error" 0 "fact" 1 "fail" 1 "ns" 1 "pass" 0 "to-do" 0)))

                    (spy-on 'cider-current-ns :and-return-value "octocat.math")
                    (spy-on 'emidje-send-test-request :and-call-through)
                    (spy-on 'cider-ensure-connected)
                    (spy-on 'cider-nrepl-send-request :and-call-fake (lambda (request callback &optional connection)
                                                                       (funcall callback response)))

                    (emidje-run-ns-tests)
                    (setq-local report (emidje-specs-read-test-report))

                    (it "infers the test namespace in question before sending a request to nREPL middleware"
                        (expect 'emidje-send-test-request :to-have-been-called-with :ns `(ns "octocat.math-test")))

                    (it "runs all tests defined in the corresponding test namespace and displays the test report"
                        (expect report :to-be "")
                        )
                    ))
