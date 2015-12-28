(ns rivet.clj.persistence
  (:require [clojure.java.jdbc :as j]))

(defn get-lexicon [db-spec]
  (j/query db-spec "select * from lexicon"))

(defn get-x-by-y [db-spec x-field y-field target]
  (first (j/query db-spec [(str "select " x-field " from lexicon where " y-field " = ?") target])))

(defn insert! [db-spec entry]
  (first (j/insert! db-spec :lexicon entry)))

(defn update! [db-spec selection entry]
  (first (j/update! db-spec :lexicon entry selection)))

(defn delete! [db-spec selection]
  (first (j/delete! db-spec :lexicon selection)))

(defn _clear-lexicon [db-spec]
  (first (j/delete! db-spec :lexicon "")))

(defn insert-metadata! [db-spec entry]
  (first (j/insert! db-spec :metadata entry)))

(defn update-metadata! [db-spec entry]
  (first (j/update! db-spec :metadata entry "")))

(defn clear-metadata! [db-spec]
  (first (j/delete! db-spec :metadata "")))

(defmacro with-query [[bind & query] yes-body no-body]
  `(let [~bind ~@query]
     (if-not (zero? ~bind)
       ~yes-body
       ~no-body)))

(defn get-metadata [db-spec]
  (first (j/query db-spec "select * from metadata")))

(defn set-metadata [db-spec metadata]
  (with-query [success (update-metadata! db-spec metadata)]
    success
    (insert-metadata! db-spec metadata)))

(defn get-size [db-spec]
  (:size  (get-metadata db-spec)))

(defn set-size [db-spec size]
  (with-query [success (update-metadata! db-spec {:size size})]
    success
    (insert-metadata! db-spec {:size size})))

(defn get-k [db-spec]
  (:k (get-metadata db-spec)))

(defn set-k [db-spec k]
  (with-query [success (update-metadata! db-spec {:k k})]
    success
    (insert-metadata! db-spec {:k k})))

(defn get-cr [db-spec]
  (:cr (get-metadata db-spec)))

(defn set-cr [db-spec cr]
  (with-query [success (update-metadata! db-spec {:cr cr})]
    success
    (insert-metadata! db-spec {:cr cr})))

(defn initialize-db [db-spec table {:keys [size k context-radius]}]
  (let [metadata-table [:metadata
                        [:size :int]
                        [:k    :int]
                        [:cr   :int]]]
    (j/db-do-commands db-spec
                      (apply j/create-table-ddl table)
                      (apply j/create-table-ddl metadata-table))
    (when-not (nil? size) (set-size db-spec size))
    (when-not (nil? k) (set-k db-spec k))
    (when-not (nil? context-radius) (set-cr db-spec context-radius))))
