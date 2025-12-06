(ns day5
  (:require
   [clojure.string :refer [split-lines]]
   [utils :refer [read-input]]))

(defn parse-ids [s]
  (map parse-long (split-lines s)))

(defn parse-ranges [s]
  (map (fn [line]
         (mapv parse-long (re-seq #"\d+" line)))
       (split-lines s)))

(defn id-in-range [id [start end]]
  (<= start id end))

(defn id-in-any-range [id ranges]
  (some #(id-in-range id %) ranges))

(defn parse-input [s]
  (let [[ranges ids] (clojure.string/split s #"\n\n")]
    [(parse-ranges ranges) (parse-ids ids)]))

(defn solve1 []
  (let [[ranges ids] (parse-input (read-input 5))]
    (->> ids
         (map #(id-in-any-range % ranges))
         (filter true?)
         (count))))

(defn merge-range [[start1 end1] [start2 end2]]
  [(min start1 start2) (max end1 end2)])

(defn should-merge [[_ end1] [start2 _]]
  (>= end1 start2))

(defn merge-ranges [ranges]
  (reduce
   (fn [result range]
     (let [current (peek result)]
       (if (should-merge current range)
         (conj (pop result) (merge-range current range))
         (conj result range))))
   [(first ranges)]
   (rest ranges)))

(defn solve2 []
  (let [[ranges _] (parse-input (read-input 5))]
    (->> (sort-by first ranges)
         (merge-ranges)
         (map (fn [[start end]] (inc (- end start))))
         (reduce +))))

(comment
  (solve2)
  (solve1)
  :rcf) 