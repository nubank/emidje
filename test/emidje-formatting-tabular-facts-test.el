;;; emidje-formatting-tabular-facts --- Tests for Emidje -*- lexical-binding: t -*-

;; Copyright © 2018 Nubank

;; This file is not part of GNU Emacs

;;; Commentary:

;; Tests for Emidje.

;;; Code:

(require 'buttercup)
(require 'emidje)
(require 'emidje-test-helpers)

(describe "When I open a Clojure test file, move the point to a tabular fact and call `emidje-format-tabular'"
          (let ((response (nrepl-dict "status" (list "done")
                                      "formatted-code"
                                      "(tabular (fact \"mixes colors\"
                             (colors/mix ?x ?y) => ?result)
              ?x      ?y ?result
           :blue :yellow  :green
         :yellow    :red :orange)")))

            (it "formats the tabular fact at point"
                (emidje-tests-with-temp-buffer "(ns octocat.colors-test
  (:require  [midje.sweet :refer :all]
             [octocat.colors :as colors]))

(facts \"about colors\"

       (tabular (fact \"mixes colors\"
                      (colors/mix ?x ?y) => ?result)
                ?x ?y ?result
                :blue :yellow :green
                :yellow :red :orange))"
                                               (forward-line 6)
                                               (right-char 7)
                                               (spy-on 'emidje-send-request :and-return-value response)
                                               (emidje-format-tabular)
                                               (expect (buffer-string) :to-equal
                                                       "(ns octocat.colors-test
  (:require  [midje.sweet :refer :all]
             [octocat.colors :as colors]))

(facts \"about colors\"

       (tabular (fact \"mixes colors\"
                             (colors/mix ?x ?y) => ?result)
              ?x      ?y ?result
           :blue :yellow  :green
         :yellow    :red :orange))")))))
