(ns rivet.clj.core.db
  (:require [rivet.persistence       :as    db]
            [rivet.persistence.words :refer [set-word-entry]]
            [rivet.util              :refer [read-str]]))

(defn read-lexicon [db-spec]
  (let [lexicon (->> db-spec
                     (db/get-lexicon)
                     (map #(assoc {} (:word %) (read-str (:lex %))))
                     (apply merge))
        metadata (db/get-metadata db-spec)]
    {:metadata metadata :lexicon lexicon}))

(defn write-lexicon [db-spec lexicon]
  (run! #(set-word-entry db-spec {:word (key %) :lex (val %)}) (:lexicon lexicon))
  (db/set-metadata db-spec (:metadata lexicon)))
