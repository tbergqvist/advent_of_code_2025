(ns day2
  (:require
   [clojure.string :refer [split]]
   [utils :refer [read-input]]))

(defn id-to-first-half [id]
  (let [half-id (->> (split-at (quot (count id) 2) id)
                     (first)
                     (apply str))]
    (cond
      (= 1 (count id)) "1"
      (even? (count id)) half-id
      :else half-id)))

(defn id-to-first-half [id]
  (let [half-length (quot (count id) 2)]
    (subs id 0 (max 1 half-length))))

(defn parse-id-range [s]
  (let [[start end] (split s #"-")]
    [(parse-long start)
     (parse-long end)
     (parse-long (id-to-first-half start))]))

(defn get-id-from-half [half-id]
  (->> (str half-id half-id)
       (parse-long)))

(defn find-invalid-ids [[start end half-id]]
  (let [id (get-id-from-half half-id)]
    (cond
      (> id end) '()
      (< id start) (find-invalid-ids [start end (inc half-id)])
      :else (conj (find-invalid-ids [start end (inc half-id)]) (list id)))))

(defn parse-line [line]
  (->> (split line #",")
       (map parse-id-range)))

(defn solve1 []
  (->> (read-input 2)
       (parse-line)
       (map find-invalid-ids)
       (flatten)
       (reduce +)))

(defn id-is-valid [id]
  (let [id-str (str id)]
    (nil? (re-matches #"^(.+)\1+$" id-str))))

(defn get-ids [line]
  (let [[start end] (map parse-long (split line #"-"))]
    (range start (inc end))))

(defn solve2 []
  (let [input (read-input 2)
        lines (split input #",")]
    (->> (map get-ids lines)
         (flatten)
         (filter (comp not id-is-valid))
         (reduce +))))

(comment
  (solve1)
  (solve2)
  :rcf)
