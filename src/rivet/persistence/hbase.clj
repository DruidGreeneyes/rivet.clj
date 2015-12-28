(ns rivet.clj.persistence.hbase
  (:require [rivet.util :as u])
  (:import [org.apache.hadoop.hbase HBaseConfiguration HTableDescriptor KeyValue TableName]
           [org.apache.hadoop.hbase.client ClientScanner ConnectionFactory Result HTable HBaseAdmin Scan Put Get ]
           [org.apache.hadoop.hbase.util Bytes]))

(defn string-to-bytes [string]
  (Bytes/toBytes string))

(defn bytes-to-string [bytes]
  (Bytes/toString bytes))

;;Tests
(def conn (ConnectionFactory/createConnection (HBaseConfiguration/create)))
(def admin (.getAdmin conn))
(def htd (u/flip get 0 (vec (.listTables admin))))
