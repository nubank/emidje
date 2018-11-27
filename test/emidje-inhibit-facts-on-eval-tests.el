;;; emidje-inhibit-facts-on-eval-tests --- Tests for Emidje -*- lexical-binding: t -*-

;; Copyright © 2018 Nubank

;; This file is not part of GNU Emacs

;;; Commentary:

;; Tests for Emidje.

;;; Code:

(require 'buttercup)
(require 'emidje)
(require 'emidje-test-helpers)

(describe "When I call `emidje-toggle-load-facts-on-eval' with a prefix argument"
          (after-all
           (setq-default emidje-load-facts-on-eval nil))

          (it "turns on/off `emidje-load-facts-on-eval' globally"
              (emidje-toggle-load-facts-on-eval t)
              (expect (emidje-tests-last-displayed-message) :to-equal "Turned on emidje-load-facts-on-eval globally")
              (emidje-tests-with-temp-buffer "(ns octocat.math-test)"
                                             (expect emidje-load-facts-on-eval :to-be t))
              (emidje-toggle-load-facts-on-eval t)
              (expect (emidje-tests-last-displayed-message) :to-equal "Turned off emidje-load-facts-on-eval globally")
              (emidje-tests-with-temp-buffer "(ns octocat.colors-test)"
                                             (expect emidje-load-facts-on-eval :to-be nil))))

(describe "When I call `emidje-toggle-load-facts-on-eval'"
          (after-all
           (setq emidje-load-facts-on-eval nil))

          (it "turns on/off `emidje-load-facts-on-eval' locally"
              (emidje-toggle-load-facts-on-eval)
              (expect (emidje-tests-last-displayed-message) :to-equal "Turned on emidje-load-facts-on-eval locally")
              (expect emidje-load-facts-on-eval :to-be t)
              (emidje-toggle-load-facts-on-eval)
              (expect (emidje-tests-last-displayed-message) :to-equal "Turned off emidje-load-facts-on-eval locally")
              (expect emidje-load-facts-on-eval :to-be nil)))

(describe "When I instrument the `nrepl-send-sync-request' function"
          (before-each
           (spy-on 'nrepl-send-sync-request))

          (describe "and `emidje-load-facts-on-eval' is set to nil"

                    (it "delegates to the original function with no changes"
                        (expect emidje-load-facts-on-eval :to-be nil)
                        (emidje-instrumented-nrepl-send-request #'nrepl-send-sync-request `("op" "eval"))
                        (expect 'nrepl-send-sync-request :to-have-been-called-with `("op" "eval"))))

          (describe "and `emidje-load-facts-on-eval' is set to t"
                    (before-all
                     (emidje-toggle-load-facts-on-eval))
                    (after-all
                     (emidje-toggle-load-facts-on-eval))

                    (it "delegates to the original function by appending the parameter `load-tests?' when the op is `eval'"
                        (expect emidje-load-facts-on-eval :to-be t)
                        (emidje-instrumented-nrepl-send-request #'nrepl-send-sync-request `("op" "eval"
                                                                                            "code" "(+ 1 2)"))
                        (expect 'nrepl-send-sync-request :to-have-been-called-with `("op" "eval"
                                                                                     "code" "(+ 1 2)"
                                                                                     "load-tests?" "true")))

                    (it "delegates to the original function by appending the parameter `load-tests?' when the op is `load-file'"
                        (expect emidje-load-facts-on-eval :to-be t)
                        (emidje-instrumented-nrepl-send-request #'nrepl-send-sync-request `("op" "load-file"
                                                                                            "file" "/home/john-doe/projects/octocat/test/octocat/math_test.clj"))
                        (expect 'nrepl-send-sync-request :to-have-been-called-with `("op" "load-file"
                                                                                     "file" "/home/john-doe/projects/octocat/test/octocat/math_test.clj"
                                                                                     "load-tests?" "true")))

                    (it "delegates to the original function with no changes when the op is any other"
                        (expect emidje-load-facts-on-eval :to-be t)
                        (emidje-instrumented-nrepl-send-request #'nrepl-send-sync-request `("op" "complete"
                                                                                            "code" "map"))
                        (expect 'nrepl-send-sync-request :to-have-been-called-with `("op" "complete"
                                                                                     "code" "map")))))

(describe "When I instrument the `nrepl-send-request' function"
          (before-each
           (spy-on 'nrepl-send-request))

          (describe "and `emidje-load-facts-on-eval' is set to nil"

                    (it "delegates to the original function with no changes"
                        (expect emidje-load-facts-on-eval :to-be nil)
                        (emidje-instrumented-nrepl-send-request #'nrepl-send-request `("op" "eval") #'identity)
                        (expect 'nrepl-send-request :to-have-been-called-with `("op" "eval") #'identity)))

          (describe "and `emidje-load-facts-on-eval' is set to t"
                    (before-all
                     (emidje-toggle-load-facts-on-eval))
                    (after-all
                     (emidje-toggle-load-facts-on-eval))

                    (it "delegates to the original function by appending the parameter `load-tests?' when the op is `eval'"
                        (expect emidje-load-facts-on-eval :to-be t)
                        (emidje-instrumented-nrepl-send-request #'nrepl-send-request `("op" "eval"
                                                                                       "code" "(+ 1 2)") #'identity)
                        (expect 'nrepl-send-request :to-have-been-called-with `("op" "eval"
                                                                                "code" "(+ 1 2)"
                                                                                "load-tests?" "true") #'identity))

                    (it "delegates to the original function by appending the parameter `load-tests?' when the op is `load-file'"
                        (expect emidje-load-facts-on-eval :to-be t)
                        (emidje-instrumented-nrepl-send-request #'nrepl-send-request `("op" "load-file"
                                                                                       "file" "/home/john-doe/projects/octocat/test/octocat/math_test.clj") #'identity)
                        (expect 'nrepl-send-request :to-have-been-called-with `("op" "load-file"
                                                                                "file" "/home/john-doe/projects/octocat/test/octocat/math_test.clj"
                                                                                "load-tests?" "true") #'identity))

                    (it "delegates to the original function with no changes when the op is any other"
                        (expect emidje-load-facts-on-eval :to-be t)
                        (emidje-instrumented-nrepl-send-request #'nrepl-send-request `("op" "complete"
                                                                                       "code" "map") #'identity)
                        (expect 'nrepl-send-request :to-have-been-called-with `("op" "complete"
                                                                                "code" "map") #'identity))))
