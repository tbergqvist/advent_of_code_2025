(ns day3
  (:require
   [clojure.string :refer [split-lines]]
   [utils :refer [read-input]]))

(defn char->long [^Character c]
  (parse-long (str c)))

(defn for-each [s start end]
  (->> (range start end)
       (map (fn [i] [i (nth s i)]))))

(defn max-key-first [k & rest]
  (apply max-key k (reverse rest)))
  ;(reduce (fn [cur-max val] (if (> (k val) (k cur-max)) val cur-max)) rest))

(defn find-highest [s start end]
  (->> (for-each s start end)
       (map #(update % 1 char->long))
       (apply max-key-first second)))

(defn find-highest-number [s]
  (let [[first-i first-val] (find-highest s 0 (dec (count s)))
        [_ second-val] (find-highest s (inc first-i) (count s))]
    (parse-long (str first-val second-val))))

(comment
  (->> (read-input 3)
       (split-lines)
       (map find-highest-number)
       (reduce +))

  (find-highest-number "811384523523527")
  (max-key-first second [5 4] [7 78] [8 1] [5 78])

  (apply max-key first [[1 2] [4 1]])
  (char->long \5)
  (get "12345" 2)
  :rcf)
