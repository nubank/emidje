(ns octocat.arithmetic-test
  (:require [midje.sweet :refer :all]))

(facts "about arithmetic operations"

       (fact (* 2 5) => 10)

       (fact "this is a crazy arithmetic"
             (+ 2 3) => {:a 1})

       (fact "two assertions in the same fact; the former is correct while the later is wrong"
             (+ 10 1) => 11
             (- 4 2) => 3)

       (fact "this will throw an unexpected exception"
             (/ 12 0) => 0)

       (tabular (fact "about basic arithmetic operations"
                      (?operation ?a ?b) => ?result)
                ?operation   ?a  ?b ?result
                +    2   5      10
                +   10   4      14
                -  100  25      75
                *  123  69    8487
                /   15   8    15/8
                / 4284 126      34 ))
