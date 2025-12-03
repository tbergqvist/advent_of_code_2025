(ns day1
  (:require
   [clojure.string :refer [split-lines]]
   [utils :refer [read-input]]))

(defn parse-rotation [s]
  (let [[[direction] num-string] (split-at 1 s)]
    [direction (parse-long (apply str num-string))]))

(defn calc-rotations [prev-value new-value]
  (let [rotations (cond
                    (> new-value 0) (quot new-value 100)
                    :else (+ 1 (quot (abs new-value) 100)))]
    (if (and (= prev-value 0) (< new-value 0))
      (dec rotations)
      rotations)))

(defn rotate [rotations [direction amount]]
  (let [prev-value (get (last rotations) 0)
        result (if (= direction \R)
                 (+ prev-value amount)
                 (- prev-value amount))]
    (conj rotations [(mod result 100)
                     (calc-rotations prev-value result)])))


(defn run-part1 []
  (->> (split-lines (read-input 1))
       (map parse-rotation)
       (reduce rotate [[50 0]])
       (map first)
       (filter zero?)
       (count)))

(defn run-part2 []
  (->> (split-lines (read-input 1))
       (map parse-rotation)
       (reduce rotate [[50 0]])
       (map second)
       (reduce +)))

(comment
  (run-part1)
  (run-part2)

  (->> ["L68"
        "L30"
        "R48"
        "L5"
        "R60"
        "L55"
        "L1"
        "L99"
        "R14"
        "L82"]
       (map parse-rotation)
       (reduce rotate [[50 0]])
       ;(map second)
       ;(reduce +)
       ;(filter #(= 0 %))
       ;(count)
       )
  :rcf)
