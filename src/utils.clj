(ns utils)

(defn read-input [day]
  (slurp (str "./inputs/" day ".txt")))

(defn char->long [^Character c]
  (parse-long (str c)))

(defn max-key-first [k & rest]
  (apply max-key k (reverse rest)))

(defn is-within-board [x y board]
  (and (>= x 0)
       (< x (count (first board)))
       (>= y 0)
       (< y (count board))))