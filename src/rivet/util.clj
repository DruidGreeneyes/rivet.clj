(ns rivet.clj.util)

(defn get-matching-keys [hash-a hash-b]
  (clojure.set/intersection (set (keys hash-a)) (set (keys hash-b))))

(defn map-intersection [fun hash-a hash-b]
  (let [ks (get-matching-keys hash-a hash-b)]
    (->> ks
         (map #(fun (hash-a %) (hash-b %)))
         (zipmap ks)
         (into {}))))

(defmacro flip [fun & args]
  `(~fun ~@(reverse args)))

(defn word-at-index [i text]
  (nth text i nil))

(defmacro log-and-return
  ([body]
   (log-and-return "" body))
  ([string body]
   `(let [~'ret ~body]
      (rivet.files/append-to-file "log.txt" (str ~string ~'ret))
      ~'ret)))

(defn some-map [pred fun coll]
  (->> coll
       (map fun)
       (some pred)))

(defn range-neg-to-pos [n]
  (range (- n) (inc n)))

(defn read-str [string]
  (with-in-str string
    (read)))

(defn between [start num end]
  (and (<= start num)
       (< num end)))

(defn center-element [v]
  (-> v
      (count)
      (/ 2)
      (int)
      (v)))

(defn but-center [v radius]
  (concat (take radius v) (take-last radius v)))

(defn ends-with [string suff]
  (.endsWith string suff))

(defn substr [string start len]
  (.substring string start len))

(defn letter? [char]
  (Character/isLetter char))

(defn truncate
  ([string num] (truncate string num false))
  ([string num from-begin]
   (let [len (- (count string) num)
         fun (if from-begin
               take-last
               take)]
     (clojure.string/join (fun len string)))))

(defn cvc-vowel? [char]
  (.contains [\a \e \i \o \u] char))

(defn vowel? [char]
  (or (cvc-vowel? char)
      (= \y char)))

(defn ends-with-cvc [string]
  (let [a (get string (dec (count string)))
        b (get string (- (count string) 2))
        c (get string (- (count string) 3))]
    (and (< 2 (count string))
         (not (.contains [\w \x \y] a))
         (not (cvc-vowel? a))
         (cvc-vowel? b)
         (not (cvc-vowel? c)))))

(defn ends-with-double-consonant [string]
  (not-any? vowel? (take-last 2 string)))

(defn number-of [pred seq]
  (let [predicate (fn [c item]
                    (if (pred item)
                      (inc c)
                      c))]
    (reduce predicate 0 seq)))

(defn cvc-measure [string]
  (number-of cvc-vowel? string))

(defn insignificant-char? [char]
  (not (or (Character/isLetter char)
           (= char \')
           (= char \-)
           (= char \/))))

(defn take-by[keyfn number coll]
  (loop [lis coll
         res (cons [(first coll) (keyfn (first coll))] nil)]
    (if (empty? lis)
      (map first res)
      (let [s   (keyfn (first lis))
            h   (take-while #(< s (second %)) res)
            t   (drop-while #(< s (second %)) res)
            result (if (nil? t)
                     h
                     (->> t
                          (cons [(first lis) (keyfn (first lis))])
                          (drop-last (if (> number (count res))
                                       0
                                       1))
                          (concat h)))]
        (recur (rest lis) result)))))
