;; Copyright © 2014 Robin Heggelund Hansen.
;; Distributed under the MIT License (http://opensource.org/licenses/MIT).

(ns differ.core-test
  (:require [differ.core :as core]
            #+clj [clojure.test :refer [is deftest testing]]
            #+cljs [cemerick.cljs.test :as t])
  #+cljs (:require-macros [cemerick.cljs.test :refer [is deftest testing]]))

(let [state {:one 1
             :two {:three 3
                   :four "test"}}
      new-state {:one 2
                 :five "5"
                 :two {:four "nice"}}
      alter {:one 2
             :five "5"
             :two {:four "nice"}}
      remo {:two {:three 0}}]

  (deftest diff
    (is (= [alter remo] (core/diff state new-state))))

  (deftest patch
    (is (= new-state (core/patch state [alter remo])))))
