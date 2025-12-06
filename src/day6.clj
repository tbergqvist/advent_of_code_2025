(ns day6
  (:require
   [clojure.string :refer [split-lines trim]]
   [utils :refer [read-input]]
   [clojure.string :as string]))

(defn get-func [s]
  (if (= (trim s) "*") * +))

(defn parse-rows [start symbols number-lines]
  (if (empty? symbols) []
      (let [symbol (first symbols)
            len (dec (count symbol))
            parse-lines (mapv (fn [line]
                                (subs line start (+ start len)))
                              number-lines)]
        (cons parse-lines (parse-rows (+ start (inc len)) (rest symbols) number-lines)))))

(defn parse-input [s]
  (let [lines (split-lines s)
        number-lines (butlast lines)
        symbol-line (last lines)
        symbols (re-seq #"[*+] *" symbol-line)
        columns (parse-rows 0 symbols number-lines)]
    (map (fn [col symbol]
           [(get-func symbol) col])
         columns
         symbols)))

(defn solve1 []
  (->> (parse-input (read-input 6))
       (map (fn [[operator columns]]
              (->> columns
                   (map (comp parse-long trim))
                   (apply operator))))
       (reduce +)))

(defn solve2 []
  (->> (parse-input (read-input 6))
       (map (fn [[operator columns]]
              (->> columns
                   (apply map str)
                   (map (comp parse-long trim))
                   (apply operator))))
       (reduce +)))

(comment
  (solve1)
  (solve2)
  :rcf) 