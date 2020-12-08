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

;;Indexing Sequences
(defn Ind-seq [coll]
  (keep-indexed (fn [x1 x2] [x2 x1]) coll))

;;Infix Calculator
(defn infix [& exp]
  (let [numb (filter number? exp)
        op (filter (complement number?) exp)]
    (loop [acc (first numb)
           number (rest numb)
           operator op]
      (if (empty? number)
        acc
        (recur ((first operator) acc (first number)) (rest number) (rest operator))))))

(defn infix* [& args]
  (loop [[x op y & exp] args]
    (let [result (op x y)]
      (if (empty? exp)
       result
       (recur (cons result exp))))))

;;Re-implement Map
(defn map* [f coll]
   (if (empty? coll)
     '()
     (lazy-seq (cons (f (first coll)) (map* f (rest coll))))))

;;sum of square digits
(defn sum-sq-dg [coll]
  (count (filter (fn [x]
             (< x (->> (seq (str x))
                       (map #(- (int %) (int \0)))
                       (map #(* % %))
                       (apply +)
                       )))
           coll)))

;;Recognize Playing Cards
(defn read-card [[suit rank]]
  (let [suit-map (zipmap [\S \H \D \C] [:spade :heart :diamond :club])
        rank-map (zipmap (concat (seq "23456789") '(\T \J \Q \K \A)) (range 13))]
    {:suit (get suit-map suit) :rank (get rank-map rank)}
    ))
