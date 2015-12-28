(ns rivet.clj.test
  (:require  [clojure.tools.trace      :refer :all]
             [rivet.persistence        :as    db]
             [rivet.files              :as    f]
             [rivet.stemmers.stopwords :refer [remove-stops]]))

(def text (f/tokenize-file "test.txt"))
;;(def texts (f/map-directory f/tokenize-file "data/enron-clean/set"))
;;(def all-texts (f/map-directory f/tokenize-file "data/enron-clean/all"))
;;(def stopped-text (remove-stops (first texts)))

(def test-lexicon-table [:lexicon
                         [:word :text]
                         [:lex]])
(def test-lexicon {:classname   "org.sqlite.JDBC"
                   :subprotocol "sqlite"
                   :subname     "db/word-lexicon.db"})

(defn initialize-test-lexicon []
  (db/initialize-db test-lexicon test-lexicon-table {:size 16000 :k 30 :context-radius 3}))
