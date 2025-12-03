(ns day3
  (:require
   [clojure.string :refer [split-lines]]
   [utils :refer [read-input char->long max-key-first]]))

(defn find-highest [s start end]
  (->> (range start end)
       (map (fn [i] [i (char->long (nth s i))]))
       (apply max-key-first second)))

(defn find-highest-number
  ([s batteries-left] (find-highest-number s 0 batteries-left))
  ([s start batteries-left]
   (if (zero? batteries-left) ""
       (let [end (- (count s) (dec batteries-left))
             [i val] (find-highest s start end)]
         (str val (find-highest-number s (inc i) (dec batteries-left)))))))

(defn solve [batteries]
  (->> (read-input 3)
       (split-lines)
       (map #(find-highest-number % batteries))
       (map parse-long)
       (reduce +)))

(comment
  (solve 2)
  (solve 12)
  :rcf)
