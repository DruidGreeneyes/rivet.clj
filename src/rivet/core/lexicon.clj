(ns rivet.clj.core.lexicon
  (:require [rivet.core.hash-labels :refer [add-labels
                                            generate-label
                                            similarity]]
            [rivet.util             :refer [but-center
                                            word-at-index
                                            range-neg-to-pos
                                            take-by]]
            [rivet.core.db          :refer [read-lexicon
                                            write-lexicon]]
            [rivet.test             :as    t]))

(defn get-size [lexicon]
  (:size (:metadata lexicon)))

(defn get-k [lexicon]
  (:k (:metadata lexicon)))

(defn get-cr [lexicon]
  (:cr (:metadata lexicon)))

(defn get-word-ind [lexicon word]
  (let [s (get-size lexicon)
        k (get-k lexicon)]
    (generate-label s k word)))

(defn get-or-make-word-entry [lexicon word]
  (let [entry (find (:lexicon lexicon) word)]
    (if-not (nil? entry)
      entry
      (find {word (get-word-ind lexicon word)} word))))

(defn get-word-lex [lexicon word]
  (val (get-or-make-word-entry lexicon word)))

(defn get-context-window [i cr text]
  (remove nil? (map #(word-at-index (+ i %) text)
                    (but-center (range-neg-to-pos cr) cr))))

(defn train-word-from-context [lexicon word context]
  (let [word-lex     (get-word-lex lexicon word)
        context-inds (map #(get-word-ind lexicon %) context)]
    (reduce add-labels word-lex context-inds)))

(defn train-lexicon-from-text [lexicon tokenized-text]
  (let [cr (get-cr lexicon)
        l  (:lexicon lexicon)]
    (letfn [(trainer [index lex]
              (let [word (word-at-index index tokenized-text)]
                (if (nil? word)
                  lex
                  (let [context (get-context-window index cr tokenized-text)]
                    (trainer (inc index)
                             (assoc lex word (train-word-from-context lexicon word context)))))))]
      (assoc lexicon :lexicon (trainer 0 l)))))

(defn train-lexicon-from-batch [lexicon tokenized-texts]
  (reduce train-lexicon-from-text lexicon tokenized-texts))

(defn lex-document [lexicon tokenized-text]
  (->> tokenized-text
       (map (partial get-word-lex lexicon))
       (reduce add-labels)))

(defn get-possible-topics-for-document [lexicon tokenized-text]
  (let [document   (lex-document lexicon tokenized-text)]
    (->> (:lexicon lexicon)
         (take-by 10 #(similarity document (val %)))
         (keys))))
