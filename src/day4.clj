(ns day4
  (:require
   [clojure.string :refer [split-lines]]
   [utils :refer [read-input]]))

(defn is-valid-coordinate [x y board]
  (and (>= x 0)
       (< x (count (first board)))
       (>= y 0)
       (< y (count board))))

(defn find-neighbours [x y board]
  (filterv (fn [[x y]] (is-valid-coordinate x y board))
           [[(dec x) (dec y)]
            [x (dec y)]
            [(inc x) (dec y)]
            [(inc x) y]
            [(inc x) (inc y)]
            [x (inc y)]
            [(dec x) (inc y)]
            [(dec x) y]]))

(defn count-neighbours [x y board]
  (->> (find-neighbours x y board)
       (map (fn [[x y]] (get-in board [y x])))
       (filter true?)
       (count)))

(defn parse-line [s]
  (mapv (fn [c] (= c \@)) s))

(defn get-board []
  (->> (read-input 4)
       (split-lines)
       (mapv parse-line)))

(defn total-rolls [board]
  (count (filter true? (flatten board))))

(defn update-roll [board x y]
  (and (get-in board [y x])
       (>= (count-neighbours x y board) 4)))

(defn update-board [board]
  (mapv (fn [y]
          (mapv (fn [x] (update-roll board x y))
                (range (count (first board)))))
        (range (count board))))

(defn count-removed [old new]
  (- (total-rolls old) (total-rolls new)))

(defn solve2 []
  (->> (iterate update-board (get-board))
       (partition 2 1)
       (map (fn [[old new]] (count-removed old new)))
       (take-while pos?)
       (reduce +)))

(defn solve1 []
  (let [board (get-board)]
    (->> (update-board board)
         (count-removed board))))

(comment
  (solve2)
  (solve1)
  :rcf) 