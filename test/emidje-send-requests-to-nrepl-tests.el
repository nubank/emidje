;;; emidje-send-requests-to-nrepl-tests --- Tests for Emidje -*- lexical-binding: t -*-

;; Copyright © 2018 Nubank

;; This file is not part of GNU Emacs

;;; Commentary:

;; Tests for Emidje.

;;; Code:

(require 'buttercup)
(require 'emidje)
(require 'emidje-test-helpers)

(defun emidje-tests-make-fake-cider-send-request (response)
  (lambda (request callback)
    (funcall callback response)))

(defun emidje-tests-fake-handler (response)
  )

(describe "When I call `emidje-send-request'"
          (before-each
           (spy-on 'cider-ensure-connected))
          (let ((succeeded-response (nrepl-dict "status" (list "done"))))

            (it "always ensure there is a Cider connection"
                (spy-on 'cider-nrepl-send-sync-request)
                (emidje-send-request :version)
                (expect 'cider-ensure-connected :to-have-been-called))

            (it "sends a synchronous request to nREPL server and returns the response"
                (spy-on 'cider-nrepl-send-sync-request :and-return-value succeeded-response)
                (expect (emidje-send-request :ns `(ns "octocat.math-test")) :to-be succeeded-response)
                (expect 'cider-nrepl-send-sync-request :to-have-been-called-with `("op" "midje-test-ns"
                                                                                   "ns" "octocat.math-test")))

            (it "translates the op alias used internally to a valid nREPL op"
                (spy-on 'cider-nrepl-send-sync-request :and-return-value succeeded-response)
                (emidje-send-request :version)
                (expect 'cider-nrepl-send-sync-request :to-have-been-called-with `("op" "midje-nrepl-version"))
                (emidje-send-request :format-tabular)
                (expect 'cider-nrepl-send-sync-request :to-have-been-called-with `("op" "midje-format-tabular"))
                (emidje-send-request :project)
                (expect 'cider-nrepl-send-sync-request :to-have-been-called-with `("op" "midje-test-all"))
                (emidje-send-request :ns)
                (expect 'cider-nrepl-send-sync-request :to-have-been-called-with `("op" "midje-test-ns"))
                (emidje-send-request :test-at-point)
                (expect 'cider-nrepl-send-sync-request :to-have-been-called-with `("op" "midje-test"))
                (emidje-send-request :retest)
                (expect 'cider-nrepl-send-sync-request :to-have-been-called-with `("op" "midje-retest"))
                (emidje-send-request :test-stacktrace)
                (expect 'cider-nrepl-send-sync-request :to-have-been-called-with `("op" "midje-test-stacktrace")))

            (it "throws an error when I pass an unknown op alias"
                (spy-on 'cider-nrepl-send-sync-request :and-return-value succeeded-response)
                (expect (emidje-send-request :unknown) :to-throw)
                (expect 'cider-nrepl-send-sync-request :not :to-have-been-called))

            (it "sends a asynchronous request to nREPL server when I pass a callback"
                (spy-on 'cider-nrepl-send-request :and-call-fake (emidje-tests-make-fake-cider-send-request succeeded-response))
                (spy-on 'emidje-tests-fake-handler)
                (emidje-send-request :ns `(ns "octocat.math-test") #'emidje-tests-fake-handler)
                (expect 'emidje-tests-fake-handler :to-have-been-called-with succeeded-response))

            (describe "and the nREPL middleware returns an error"

                      (it "shows an error message when there is one"
                          (spy-on 'cider-nrepl-send-request :and-call-fake (emidje-tests-make-fake-cider-send-request (nrepl-dict "status" (list "done" "error")
                                                                                                                                  "error-message" "I don't believe it!")))
                          (spy-on 'emidje-tests-fake-handler)
                          (expect (emidje-send-request :ns `() #'emidje-tests-fake-handler)
                                  :to-throw 'user-error)
                          (expect 'emidje-tests-fake-handler :not :to-have-been-called)))))
