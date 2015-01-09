;; Copyright © 2014 Robin Heggelund Hansen.
;; Distributed under the MIT License (http://opensource.org/licenses/MIT).

(ns differ.patch
  "Use the functions in this namespace to apply diffs, created by functions
in the differ.diff namespace, to similar datastructures."
  (:require [clojure.set :as set]
            [differ.hierarchy :as h]))

(defmulti alterations
  "Returns a new datastructure, containing the changes in the provided diff."
  (fn [state diff] [(h/tag-coll state) (h/tag-coll diff)])
  :hierarchy h/h)
(defmethod alterations [:differ.hierarchy/map :differ.hierarchy/map] [state diff]
  (loop [[k & ks] (keys diff)
         result (transient state)]
    (if-not k
      (persistent! result)
      (let [old-val (get result k)
            diff-val (get diff k)]
        (recur ks (assoc! result k (alterations old-val diff-val)))))))
(defmethod alterations [:differ.hierarchy/seq :differ.hierarchy/vec] [state diff]
  (loop [idx 0
         [old-val & old-rest :as old-coll] state
         [diff-idx diff-val & diff-rest :as diff-coll] diff
         result (transient [])]
    (let [old-empty? (empty? old-coll)
          diff-empty? (empty? diff-coll)]
      (cond (and old-empty? diff-empty?)
            (persistent! result)

            diff-empty?
            (recur (inc idx) old-rest diff-rest (conj! result old-val))

            (or (= idx diff-idx) old-empty?)
            (recur (inc idx) old-rest diff-rest (conj! result (alterations old-val diff-val)))

            :else
            (recur (inc idx) old-rest diff-coll (conj! result old-val))))))
(defmethod alterations [:differ.hierarchy/seq :differ.hierarchy/seq] [state diff]
  (into (list) (reverse (alterations state (vec diff)))))
(defmethod alterations [:differ.hierarchy/set :differ.hierarchy/set] [state diff]
  (set/union state diff))
(defmethod alterations :default [state diff]
  diff)
(prefer-method alterations [:differ.hierarchy/seq :differ.hierarchy/vec]
                           [:differ.hierarchy/seq :differ.hierarchy/seq])

(defmulti removals
  "Returns a new datastructure, not containing the elements in the provided diff."
  (fn [state diff] [(h/tag-coll state) (h/tag-coll diff)])
  :hierarchy h/h)
(defmethod removals [:differ.hierarchy/map :differ.hierarchy/map] [state diff]
  (loop [[k & ks] (keys diff)
         result (transient state)]
    (if-not k
      (persistent! result)
      (let [old-val (get result k)
            diff-val (get diff k)]
        (if (= 0 diff-val)
          (recur ks (dissoc! result k))
          (recur ks (assoc! result k (removals old-val diff-val))))))))
(defmethod removals [:differ.hierarchy/seq :differ.hierarchy/vec] [state diff]
  (let [max-index (- (count state) (first diff))]
    (loop [index 0
           [old-val & old-rest :as old-coll] state
           [diff-index diff-val & diff-rest :as diff-coll] (rest diff)
           result (transient [])]
      (cond (or (= index max-index) (empty? old-coll))
            (persistent! result)

            (= index diff-index)
            (recur (inc index) old-rest diff-rest (conj! result (removals old-val diff-val)))

            :else
            (recur (inc index) old-rest diff-coll (conj! result old-val))))))
(defmethod removals [:differ.hierarchy/seq :differ.hierarchy/seq] [state diff]
  (into (list) (reverse (removals state (vec diff)))))
(defmethod removals [:differ.hierarchy/set :differ.hierarchy/set] [state diff]
  (set/difference state diff))
(defmethod removals :default [state diff]
  state)
(prefer-method removals [:differ.hierarchy/seq :differ.hierarchy/vec]
                        [:differ.hierarchy/seq :differ.hierarchy/seq])
