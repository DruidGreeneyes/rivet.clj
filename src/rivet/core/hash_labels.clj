(ns rivet.clj.core.hash-labels
  (:require [rivet.util                 :refer [map-intersection]]
            [clojure.math.numeric-tower :refer [sqrt expt floor ceil]]
            [clojure.data.generators    :as    gen]))

(defn dot-product [label-a label-b]
  (let [i (vals (map-intersection * label-a label-b))]
    (if (nil? i)
      0
      (apply + i))))

(defn magnitude [label]
  (->> label
       (vals)
       (map #(expt % 2))
       (apply +)
       (sqrt)))

(defn similarity
  ([word-a word-b lexicon] (similarity (lexicon word-a) (lexicon word-b)))
  ([label-a label-b]
   (let [mag (* (magnitude label-a) (magnitude label-b))]
     (if (zero? mag)
       0
       (let [dp (dot-product label-a label-b)]
         (/ dp mag))))))

(defn multiply-label [num label]
  (map #(* num %) label))

(defn add-labels [label-a label-b]
  (merge-with + label-a label-b))

(defn make-indices [size k seed]
  (binding [gen/*rnd* (java.util.Random. seed)]
    (loop [c 0
           r []]
      (if (= c k)
        r
        (recur (inc c)
               (conj r (gen/uniform 0 size)))))))

(defn make-ks [k seed]
  (let [j (/ k 2)]
    (binding [gen/*rnd* (java.util.Random. seed)]
      (gen/shuffle (concat (repeat j 1) (repeat j -1))))))

(defn make-seed [word]
  (let [nums (map int word)]
    (->> nums
         (count)
         (range)
         (map #(* (nth nums %) (expt 10 (- %))))
         (reduce +))))

(defn generate-label [size k word]
  (let [seed (make-seed word)
        j    (if (even? k)
               k
               (inc k))]
    (zipmap (make-indices size j seed) (make-ks j seed))))

(defn generate-labels [size k]
  {:size size :k k})
