(ns rivet.clj.stemmers.truncate
  (:require [rivet.util :refer [substr]]))

(defn stem [str]
  (if (> 10 (count str))
    str
    (substr str 0 10)))
