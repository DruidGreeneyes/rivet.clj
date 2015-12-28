(ns rivet.clj.persistence.documents
  (:require [rivet.persistence       :as db]
            [rivet.persistence.words :as dw]
            [rivet.core.hash-labels  :as l]
            [rivet.files             :as f]
            [clojure.java.jdbc       :as j]))

(def test-lexicon-table [:lexicon
                         [:name   :text]
                         [:lex    :text]
                         [:topics]])
(def test-lexicon {:classname   "org.sqlite.JDBC"
                   :subprotocol "sqlite"
                   :subname     "db/document-lexicon.db"})

(defn lex-document [word-lexicon tokenized-text]
  (j/with-db-transaction [t word-lexicon]
    (let [label (l/generate-label (db/get-size t) (db/get-k t))
          new-words (dw/ensure-labels t tokenized-text)]
      (when new-words (dw/train-lexicon-from-text t tokenized-text))
      (let [words (map #(dw/get-word-lex t %) tokenized-text)]
        (reduce l/add-labels label words)))))

(defn generate-doc-entry
  ([filename lex] (generate-doc-entry filename lex nil))
  ([filename lex topics] {:name filename :lex lex :topics topics}))

(defn get-doc-entry [doc-lexicon filename]
  (db/get-x-by-y doc-lexicon "*" "name" filename))

(defn get-doc-lex [doc-lexicon filename]
  (:lex (get-doc-entry doc-lexicon filename)))

(defn get-doc-topics [doc-lexicon filename]
  (:topics (get-doc-entry doc-lexicon filename)))

(defn set-doc-entry [doc-lexicon entry]
  (db/with-query [success (db/update! doc-lexicon ["name = ?" (:name entry)] entry)]
    success
    (db/insert! doc-lexicon entry)))

(defn set-doc-lex [doc-lexicon filename lex]
  (db/with-query [success (db/update! doc-lexicon ["name = ?" filename] {:lex lex})]
    success
    (db/insert! doc-lexicon {:name filename :lex lex})))

(defn set-doc-topics [doc-lexicon filename topics]
  (db/with-query [success (db/update! doc-lexicon ["name = ?" filename] {:topics topics})]
    success
    (db/insert! doc-lexicon {:name filename :topics topics})))

(defn add-document-to-lexicon [doc-lexicon word-lexicon tokenized-doc]
  (let [filename (:name tokenized-doc)
        lex      (lex-document word-lexicon (:tokens tokenized-doc))]
    (j/with-db-transaction [t doc-lexicon]
      (set-doc-entry t (generate-doc-entry filename lex)))))

(defn add-batch-to-lexicon [doc-lexicon word-lexicon tokenized-docs]
  (run! #(add-document-to-lexicon doc-lexicon word-lexicon %) tokenized-docs))
