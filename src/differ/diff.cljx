;; Copyright © 2014 Robin Heggelund Hansen.
;; Distributed under the MIT License (http://opensource.org/licenses/MIT).

(ns differ.diff
  "Provides functions to compare two clojure datastructures and return the
difference between them. Alterations will return the elements that differ,
the removals will return elements that only exist in one collection."
  (:require [clojure.set :as set]))

(def h (-> (make-hierarchy)
           (derive ::coll ::val)
           (derive ::map  ::coll)
           (derive ::seq  ::coll)
           (derive ::set  ::coll)
           (derive ::vec  ::seq)))

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

(defmulti alterations
  "Find elements that are different in new-state, when compared to state.
  The datastructure returned will be of the same type as the first argument
  passed. Works recursively on nested datastructures."
  (fn [state new-state] [(tag-coll state) (tag-coll new-state)])
  :hierarchy #'h)
(defmethod alterations [::map ::map] [state new-state]
  (loop [[k & ks] (keys new-state)
         diff (transient {})]
    (if-not k
      (persistent! diff)
      (let [old-val (get state k)
            new-val (alterations old-val (get new-state k))]
        (cond (and (coll? new-val) (empty? new-val))
              (recur ks diff)

              (= old-val new-val)
              (recur ks diff)

              :else
              (recur ks (assoc! diff k new-val)))))))
(defmethod alterations [::seq ::vec] [state new-state]
  (loop [idx 0
         [old-val & old-rest :as old-coll] state
         [new-val & new-rest :as new-coll] new-state
         diff (transient [])]
    (if-not (seq new-coll)
      (persistent! diff)
      (let [val-diff (alterations old-val new-val)]
        (cond (empty? old-coll)
              (recur (inc idx) old-rest new-rest (conj! (conj! diff :+) val-diff))

              (= old-val new-val)
              (recur (inc idx) old-rest new-rest diff)

              :else
              (recur (inc idx) old-rest new-rest (conj! (conj! diff idx) val-diff)))))))
(defmethod alterations [::seq ::seq] [state new-state]
  (into (list) (reverse (alterations state (vec new-state)))))
(defmethod alterations [::set ::set] [state new-state]
  (set/difference new-state state))
(defmethod alterations :default [state new-state]
  new-state)

(defmulti removals
  "Find elements that are in state, but not in new-state.
  The datastructure returned will be of the same type as the first argument
  passed. Works recursively on nested datastructures."
  (fn [state new-state] [(tag-coll state) (tag-coll new-state)])
  :hierarchy #'h)
(defmethod removals [::map ::map] [state new-state]
  (let [new-keys (set (keys new-state))]
    (loop [[k & ks] (keys state)
           diff (transient {})]
      (if-not k
        (persistent! diff)
        (if-not (contains? new-keys k)
          (recur ks (assoc! diff k 0))
          (let [old-val (get state k)
                new-val (get new-state k)
                rms (removals old-val new-val)]
            (if (and (coll? rms) (seq rms))
              (recur ks (assoc! diff k rms))
              (recur ks diff))))))))
(defmethod removals [::seq ::vec] [state new-state]
  (let [diff (- (count state) (count new-state))
        empty-state []]
    (loop [idx 0
           [old-val & old-rest :as old-coll] state
           [new-val & new-rest :as new-coll] new-state
           rem (transient (conj empty-state diff))]
      (if-not (and (seq old-coll) (seq new-coll))
        (let [base (persistent! rem)]
          (if (and (= 1 (count base))
                   (>= 0 (first base)))
            empty-state
            base))
        (let [new-rem (removals old-val new-val)]
          (if (or (and (coll? new-rem) (empty? new-rem))
                  (= old-val new-rem))
            (recur (inc idx) old-rest new-rest rem)
            (recur (inc idx) old-rest new-rest (conj! (conj! rem idx) new-rem))))))))
(defmethod removals [::seq ::seq] [state new-state]
  (into (list) (reverse (removals state (vec new-state)))))
(defmethod removals [::set ::set] [state new-state]
  (set/difference state new-state))
(defmethod removals [::coll ::coll] [state new-state]
  (empty state))
(defmethod removals :default [state new-state]
  state)
