(ns rivet.clj.stemmers.porter
  (:require [rivet.util :refer :all]))

(defn step1a [s]
  (cond
    (ends-with s "sses") (truncate s 2)
    (ends-with s "ies")  (truncate s 2)
    (ends-with s "ss")   s
    (ends-with s "s")    (truncate s 1)
    :else s))

(defn step1b2 [s]
  (cond
    (or (ends-with s "at")
        (ends-with s "bl")
        (ends-with s "iz"))            (str s "e")
    (and (< 1 (count s))
         (ends-with-double-consonant s)
         (not (or (ends-with s "l")
                  (ends-with s "s")
                  (ends-with s "z")))) (truncate s 1)
    (and (= 1 (cvc-measure s))
         (ends-with-cvc s))            (str s "e")
    :else s))

(defn step1b [s]
  (cond
    (ends-with s "eed")                (if (some cvc-vowel? (truncate s 3))
                                         (truncate s 1)
                                         s)
    (and (ends-with s "ed")
         (some cvc-vowel? (truncate s 2))) (step1b2 (truncate s 2))
    (and (ends-with s "ing")
         (some cvc-vowel? (truncate s 3))) (step1b2 (truncate s 3))
    :else s))

(defn step1c [s]
  (if (and (ends-with s "y") (some vowel? (truncate s 1)))
    (str (truncate s 1) "i")
    s))

(defn step-helper [string suff trun]
  (and (ends-with string suff)
       (some cvc-vowel? (truncate string trun))))

(defn step2 [s]
  (cond
    (step-helper s "ational" 5)       (str (truncate s 5) "e")
    (or  (step-helper s "tional"  2)
         (step-helper s "enci"    2)) (truncate s 2)
    (step-helper s "anci"    1)       (str (truncate s 1) "e")
    (step-helper s "izer"    1)       (truncate s 1)
    (step-helper s "abli"    1)       (str (truncate s 1) "e")
    (or  (step-helper s "alli"    2)
         (step-helper s "entli"   2)
         (step-helper s "eli"     2)
         (step-helper s "ousli"   2)) (truncate s 2)
    (step-helper s "ization" 5)       (str (truncate s 1) "e")
    (step-helper s "ation"   3)       (str (truncate s 3) "e")
    (step-helper s "ator"    2)       (str (truncate s 2) "e")
    (step-helper s "alism"   3)       (truncate s 3)
    (or (step-helper s "iveness" 4)
        (step-helper s "fulness" 4)
        (step-helper s "ousness" 4))  (truncate s 4)
    (step-helper s "aliti"   3)       (truncate s 3)
    (step-helper s "iviti"   3)       (str (truncate s 3) "e")
    (step-helper s "biliti"  5)       (str (truncate s 5) "le")
    :else s))

(defn step3 [s]
  (cond
    (step-helper s "icate" 3)      (truncate s 3)
    (step-helper s "ative" 5)      (truncate s 5)
    (or (step-helper s "alize" 3)
        (step-helper s "iciti" 3)) (truncate s 3)
    (step-helper s "ical"  2)      (truncate s 2)
    (step-helper s "ful"   3)      (truncate s 3)
    (step-helper s "ness"  4)      (truncate s 4)
    :else s))

(defn step4 [s]
  (cond
    (step-helper s "al"    2)     (truncate s 2)
    (or (step-helper s "ance" 4)
        (step-helper s "ence" 4)) (truncate s 4)
    (or (step-helper s "er" 2)
        (step-helper s "ic" 2))   (truncate s 2)
    (or (step-helper s "able" 4)
        (step-helper s "ible" 4)) (truncate s 4)
    (step-helper s "ant"   3)     (truncate s 3)
    (step-helper s "ement" 5)     (truncate s 5)
    (step-helper s "ment"  4)     (truncate s 4)
    (or (step-helper s "end"  3)
        (step-helper s "sion" 3)
        (step-helper s "tion" 3)) (truncate s 3)
    (step-helper s "ou"    2)     (truncate s 2)
    (or (step-helper s "ism" 3)
        (step-helper s "ate" 3)
        (step-helper s "iti" 3)
        (step-helper s "ous" 3)
        (step-helper s "ive" 3)
        (step-helper s "ize" 3))  (truncate s 3)
    :else s))

(defn step5a [s]
  (let [st (truncate s 1)]
    (if (or (not (ends-with s "e"))
            (not-any? cvc-vowel? st)
            (ends-with-cvc st))
      s
      st)))

(defn step5b [s]
  (let [st (truncate s 1)]
    (if (and (ends-with "l" s)
             (ends-with-double-consonant s)
             (< 1 (cvc-measure st)))
      st
      s)))

;;primary function
(defn stem [string]
  (if (or (> 3 (count string))
          (not-every? letter? string))
    string
    (-> string
        (step1a)
        (step1b)
        (step1c)
        (step2)
        (step3)
        (step4)
        (step5a)
        (step5b))))

(defn stem-words [words]
  (map stem words))
