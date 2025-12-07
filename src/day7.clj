(ns day7
  (:require
   [clojure.set :refer [union]]
   [clojure.string :refer [split-lines]]
   [utils :refer [read-input is-within-board]]))

(defn parse-input [s]
  (->> (split-lines s)))

(defn find-char [board ^Character c]
  (->> (for [y (range (count board))
             x (range (count (first board)))
             :let [ch (get-in board [y x])]
             :when (= c ch)]
         [y x])
       (first)))

(defn is-splitter [coord board]
  (= \^ (get-in board coord)))

(defn find-splitters [[y x] board splitters]
  (cond
    (not (is-within-board x y board)) splitters
    (contains? splitters [y x]) splitters
    :else
    (if (is-splitter [y x] board)
      (let [splitters2 (conj splitters [y x])]
        (->> splitters2
             (find-splitters [(inc y) (inc x)] board)
             (find-splitters [(inc y) (dec x)] board)))
      (find-splitters [(inc y) x] board splitters))))

(declare count-paths-mem)
(defn count-paths [[y x] board]
  (cond
    (not (is-within-board x y board)) 1
    :else
    (if (is-splitter [y x] board)
      (+ (count-paths-mem [(inc y) (inc x)] board)
         (count-paths-mem [(inc y) (dec x)] board))
      (count-paths-mem [(inc y) x] board))))

(def count-paths-mem
  (memoize count-paths))

(defn solve1 []
  (let [board (parse-input (read-input 7))
        first-pos (find-char board \S)]
    (count (find-splitters first-pos board #{}))))

(defn solve2 []
  (let [board (parse-input (read-input 7))
        first-pos (find-char board \S)]
    (count-paths-mem first-pos board)))

(comment
  (solve1)
  (solve2)
  :rcf) 