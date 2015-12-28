(ns rivet.clj.core.ls-labels
  (:require [rivet.core                 :refer :all]
            [clojure.math.numeric-tower :refer [sqrt]]))

(defn dot-product [label-a label-b]
  (reduce + (map * label-a label-b)))

(defn magnitude [label]
  (sqrt (reduce + (map #(* % %) label))))

(defn similarity
  ([word-a word-b lexicon] (similarity (lexicon word-a) (lexicon word-b)))
  ([label-a label-b]
   (let [mag (* (magnitude label-a) (magnitude label-b))]
     (if (zero? mag)
       0
       (let [dp (dot-product label-a label-b)]
         (/ dp mag))))))

(defn make-k []
  (rand-nth [1 -1]))

(defn multiply-label [num label]
  (map #(* num %) label))

(defn add-labels [& labels]
  (apply (partial map +) labels))

(defn generate-label [size k]
  (shuffle (lazy-cat (repeatedly k make-k) (repeat (- size k) 0))))

(defn get-k [base-label]
  (count (remove zero? base-label)))

(defn get-size [label]
  (count label))
