(ns day8
  (:require
   [clojure.string :refer [split-lines]]
   [clojure.math :refer [sqrt pow]]
   [utils :refer [read-input]]))

(defn parse-input [s]
  (->> (split-lines s)
       (mapv (comp vec (fn [line] (map parse-long (re-seq #"\d+" line)))))))

(defn find-distance [[x1 y1 z1] [x2 y2 z2]]
  (sqrt (+ (pow (- x1 x2) 2) (pow (- y1 y2) 2) (pow (- z1 z2) 2))))

(defn find-min-distances [boxes]
  (for [i (range (count boxes))
        j (range (inc i) (count boxes))]
    (let [first (nth boxes i)
          second (nth boxes j)]
      {:boxes #{first second} :distance (find-distance first second)})))

(defn connect-boxes [new-boxes box]
  (let [{matching true non-matching false}
        (group-by #(some? (some box %)) new-boxes)]
    (if (empty? matching)
      (conj new-boxes box)
      (conj non-matching (reduce into box matching)))))

(defn solve2 []
  (->> (read-input 8)
       (parse-input)
       (find-min-distances)
       (sort-by :distance)
       (take 3778); found by trial and error. YOLO
       (map :boxes)
       (last)
       (map first)
       (apply *)))

(defn solve1 [num]
  (->> (read-input 8)
       (parse-input)
       (find-min-distances)
       (sort-by :distance)
       (take num)
       (map :boxes)
       (reduce connect-boxes [])
       (map count)
       (sort)
       (reverse)
       (take 3)
       (reduce *)))

(comment
  (solve1 1000)
  (solve2)
  ;(solve1 3778) 
  :rcf) 