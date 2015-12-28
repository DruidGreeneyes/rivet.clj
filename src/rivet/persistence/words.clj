(ns rivet.clj.persistence.words
  (:require [rivet.persistence      :as    db]
            [rivet.core.hash-labels :refer [generate-label add-labels]]
            [rivet.util             :as    u]
            [clojure.java.jdbc      :as    j]))

(defn get-word-entry [db-spec word]
  (db/get-x-by-y db-spec "*" "word" word))

(defn get-word-lex [db-spec word]
  (let [entry (get-word-entry db-spec word)]
    (when entry (u/read-str (:lex entry)))))

(defn set-word-entry [db-spec entry]
  (db/with-query [success (db/update! db-spec ["word = ?" (:word entry)] entry)]
    "Update!"
    (do (db/insert! db-spec entry)
        "Insert!")))

(defn set-word-lex [db-spec word lex]
  (db/with-query [success (db/update! db-spec ["word = ?" word] {:lex lex})]
    "Update!"
    (do (db/insert! db-spec {:word word :lex lex})
        "Insert!")))

(defn generate-word-ind [db-spec word]
  (generate-label (db/get-size db-spec) (db/get-k db-spec) word))

(defn make-word-label [db-spec word]
  (let [label (generate-word-ind db-spec word)]
    (set-word-entry db-spec {:word word :lex label})))

(defn get-or-make-word-entry [db-spec word]
  (let [entry (get-word-entry db-spec word)]
    (or entry
        (do (make-word-label db-spec word)
            (get-or-make-word-entry db-spec word)))))

(defn get-or-make-word-lex [db-spec word]
  (u/read-str (:lex (get-or-make-word-entry db-spec word))))

(defn ensure-labels [db-spec text]
  (j/with-db-transaction [t db-spec]
    (some true? (doall (map #(get-or-make-word-entry t %) text)))))

(defn train [lex context-lexes]
  (reduce add-labels lex context-lexes))

(defn train-lexicon [wlx text cr i]
  (j/with-db-transaction [t wlx]
    (let [word    (u/word-at-index i text)
          window  (remove nil? (map #(u/word-at-index (+ i %) text)
                                    (u/but-center (u/range-neg-to-pos cr) cr)))
          context (map #(generate-word-ind t %) window)
          lex     (train (get-or-make-word-lex t word) context)]
      (try
        (set-word-lex t word lex)
        (catch Exception e
          (println e))))))

(defn train-lexicon-from-text
  ([word-lexicon tokenized-text]
   (train-lexicon-from-text word-lexicon tokenized-text (db/get-cr word-lexicon)))
  ([word-lexicon tokenized-text context-radius]
   (j/with-db-transaction [t word-lexicon]
     (run! #(train-lexicon t tokenized-text context-radius %)
           (range (count tokenized-text))))))

(defn train-lexicon-from-batch [word-lexicon texts]
  (j/with-db-transaction [t word-lexicon]
    (let [cr (db/get-cr word-lexicon)]
      (run! #(train-lexicon-from-text word-lexicon % cr) texts))))
