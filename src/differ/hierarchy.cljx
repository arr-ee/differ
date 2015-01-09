;; Copyright © 2014 Robin Heggelund Hansen.
;; Distributed under the MIT License (http://opensource.org/licenses/MIT).

(ns differ.hierarchy)

(def h (-> (make-hierarchy)
           (derive ::coll ::val)
           (derive ::map  ::coll)
           (derive ::seq  ::coll)
           (derive ::set  ::coll)
           (derive ::vec  ::seq)))

;; tests vector is order-sensitive: put more specific tests on top
;; i.e. vector? before sequential?, sequential? before coll?
(def tests [[map?        ::map]
            [vector?     ::vec]
            [sequential? ::seq]
            [set?        ::set]
            [coll?       ::coll]
            [identity    ::other]])

(defn tag-coll [coll]
  (->> tests
       (filter (fn [[test-fn tag]] (test-fn coll)))
       first
       last))
