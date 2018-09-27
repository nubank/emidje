(ns octocat.colls-test
  (:require [matcher-combinators.matchers :as m]
            [matcher-combinators.midje :refer [match]]
            [midje.emission.state :refer [with-isolated-output-counters]]
            [midje.sweet :refer :all]))

(with-isolated-output-counters

  (fact "about Clojure collections"

        (fact "one key is missing in the actual map"
              {:first-name "John"} => {:first-name "John" :last-name "Doe"})

        (fact "the rightmost isn't contained into the leftmost"
              [1 2 3] => (contains [1 2 3 4])
              {:elements [:a :b]}
              => (match {:elements [:b :c]}))

        (fact "the leftmost doesn't have the same elements as the rightmost"
              [1 2 3] => (match (m/in-any-order [3 2 4])))))
-
