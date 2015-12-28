(ns rivet.clj.files
  (:require [clojure.string           :as    st]
            [clojure.java.io          :as    io]
            [rivet.stemmers.porter    :refer [stem-words]]
            [rivet.stemmers.stopwords :refer [remove-stops]]
            [rivet.util               :as    u]))

(defn tokenize-file [file]
  (->> file
       (slurp)
       (st/lower-case)
       (#(st/split % #"(_|[^\w'-])"))
       (remove #(> 3 (count %)))))

(defn process-file [file]
  (->> file
       (tokenize-file)
       (stem-words)
       (remove-stops)
       (remove #(> 3 (count %)))))

(defn append-to-file [path obj]
  (clojure.pprint/pprint obj (io/writer path :append true)))

(defn dump-to-file [obj path]
  (clojure.pprint/pprint obj (io/writer path)))

(defn read-file [file]
  (slurp file))

(defn map-directory [fun dir]
  (let [files (remove #(.isDirectory %) (file-seq (io/file dir)))
        f     (fn [x]
                (try (fun x)
                     (catch Exception e (println e))))]
    (map f files)))

(defn recursive-map-directory [fun dir]
  (let [files (file-seq (io/file dir))
        f     (fn [x]
                (if (.isDirectory x)
                  (recursive-map-directory fun x)
                  (try (fun x)
                       (catch Exception e (println e)))))]
    (map f (rest files))))

(defn reduce-directory [fun dir]
  (let [files (remove #(.isDirectory %) (file-seq (io/file dir)))
        f     (fn [x]
                (try (fun x)
                     (catch Exception e (println e))))]
    (reduce f files)))

(defn filename-from-path [path]
  (-> path
      (st/split #"/|\\")
      (last)))
