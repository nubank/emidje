(ns octocat.arithmetic-test
  (:require [midje.sweet :refer :all]))

(facts "about arithmetic operations"

       (fact (* 2 5) => 10)

       (fact "this is a crazy arithmetic"
             (+ 2 3) => 6)

       (fact "two assertions in the same fact; the former is correct while the later is wrong"
             (+ 10 1) => 11
             (- 4 2) => 3)

       (fact "this will throw an unexpected exception"
             (/ 12 0) => 0))
