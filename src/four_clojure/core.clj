(ns four-clojure.core
  (:require [clojure.set :refer :all]))

(defn zipmap* [coll1 coll2]
  (into {} (map (fn [a b] [a b]) coll1 coll2)))

(defn intersection* [set1 set2]
  (->> (difference set1 set2) (difference set1)))

(defn iterate* [f init]
  (cons init (lazy-seq (iterate* f (f init)))))