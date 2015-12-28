(ns rivet.clj.persistence.topics
  (:require [rivet.persistence             :as db]
            [rivet.persistence.words       :as pw]
            [rivet.persistence.documents   :as pd]
            [rivet.core.hash-labels        :as h]
            [rivet.util                    :as u]
            [clojure.java.jdbc             :as j]))


(def topic-lexicon-table [:lexicon
                          [:topic :text]
                          [:lex]])
(def topic-lexicon {:classname   "org.sqlite.JDBC"
                    :subprotocol "sqlite"
                    :subname     "db/topic-lexicon.db"})

(defn lex-topic [doc-lexicon topic]
  (j/with-db-transaction [t doc-lexicon]
    (let [lexes (j/query t ["select lex from lexicon where topic like ?" (str "%" topic "%")])]
      (if (empty? lexes)
        (println (str "No documents found tagged with this topic: " topic))
        (reduce h/add-labels (map #(u/read-str (:lex %))))))))

(defn get-topic-entry [topic-lexicon topic]
  (db/get-x-by-y topic-lexicon "*" "topic" topic))

(defn get-topic-lex [topic-lexicon topic]
  (let [lex (:lex (get-topic-entry topic-lexicon topic))]
    (when lex (u/read-str lex))))

(defn set-topic-entry [topic-lexicon entry]
  (db/with-query [success (db/update! topic-lexicon ["topic = ?" (:topic entry)] entry)]
    "Update!"
    (do (db/insert! topic-lexicon entry)
        "Insert!")))

(defn set-topic-lex [topic-lexicon topic lex]
  (db/with-query [success (db/update! topic-lexicon ["topic = ?" topic] {:lex lex})]
    "Update!"
    (do (db/insert! topic-lexicon {:topic topic :lex lex})
        "Insert!")))

(defn train-topic [topic-lexicon doc-lexicon topic]
  (let [lex (get-topic-lex topic-lexicon topic)
        new-lex (lex-topic doc-lexicon topic)]
    (j/with-db-transaction [t topic-lexicon]
      (set-topic-lex t topic (h/add-labels lex new-lex)))))

(defn train-topic-batch [topic-lexicon doc-lexicon topics]
  (run! #(train-topic topic-lexicon doc-lexicon %) topics))
