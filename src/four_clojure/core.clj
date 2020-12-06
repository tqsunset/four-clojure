(ns four-clojure.core
  (:require [clojure.set :refer :all]))

(defn zipmap* [coll1 coll2]
  (into {} (map (fn [a b] [a b]) coll1 coll2)))

(defn intersection* [set1 set2]
  (->> (difference set1 set2) (difference set1)))

(defn iterate* [f init]
  (cons init (lazy-seq (iterate* f (f init)))))

;;Comparisions
(defn compare* [op d1 d2]
  (let [d1<d2 (op d1 d2)
        d1>d2 (op d2 d1)]
    (cond
      (not (or d1<d2 d1>d2)) :eq
      d1<d2 :lt
      :else :gt)))

;;Product Digits
(defn prod-digit [a b]
  (->> (* a b)
      str
      seq
      (map #(- (int %) (int \0)))))

;;Group a Sequence
(defn group-by* [pred coll]
  (reduce (fn [acc x]
            (update acc
                    (pred x)
                    #(vec (conj % x))))
          {} coll))

;;Dot product
(defn dot-product [coll1 coll2]
  (apply + (map * coll1 coll2)))

;;Read a binary number
(defn read-binary [string]
  (Integer/parseInt string 2))