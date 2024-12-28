(ns aoc24.day04
  (:require
   [com.xadecimal.mutable-var :as mut :refer]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def input (slurp (io/resource "aoc24/day04.txt")))

(defn parse-grid [input]
  (str/split-lines input))

(defn- extract-string [grid row col word-len dir-row dir-col]
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [i 0
           s ""]
      (if (>= i word-len)
        s
        (let [new-row (+ row (* i dir-row))
              new-col (+ col (* i dir-col))]
          (if (and (>= new-row 0) (< new-row rows) (>= new-col 0) (< new-col cols))
            (recur (inc i) (str s (get-in grid [new-row new-col]))) ""))))))

(defn count-occurrences [grid word]
  (let [rows (count grid)
        cols (count (first grid))
        word-len (count word)
        directions [[0 1] [1 0] [1 1] [1 -1]]]
    (mut/var-scope
     (var mutable-count 0)
     (doseq [row (range rows)]
       (doseq [col (range cols)]
         (doseq [[dir-row dir-col] directions]
           (let [forward (extract-string grid row col word-len dir-row dir-col)
                 backward (str/reverse forward)]
            ;; (when (and (= row 2) (= col 3)) (sc.api/spy))
             (when (= forward word)
               (set! mutable-count (+ mutable-count 1)))
             (when (= backward word)
               (set! mutable-count (+ mutable-count 1)))))))
     mutable-count)))

(defn- check-diagonal-for-mas [grid row col dir-row dir-col]
  (let [word "MAS"
        reversed-word (str/reverse word)
        start-row (- row dir-row)
        start-col (- col dir-col)
        diagonal-str (extract-string grid start-row start-col (count word) dir-row dir-col)]
    (or (= diagonal-str word) (= diagonal-str reversed-word))))

(defn- is-xmas-shape [grid row col]
  (let [center-char (get-in grid [row col])]
    (if (not= center-char \A)
      false
      (and
       (check-diagonal-for-mas grid row col -1 -1)
       (check-diagonal-for-mas grid row col -1 1)
       (check-diagonal-for-mas grid row col 1 -1)
       (check-diagonal-for-mas grid row col 1 1)))))

(defn count-xmas-shapes [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (reduce
     (fn [count row]
       (+ count (reduce
                 (fn [col-count col]
                   (+ col-count (if (is-xmas-shape grid row col) 1 0)))
                 0 (range 1 (- cols 1)))))
     0 (range 1 (- rows 1)))))

(defn part-1 [_]
  (prn (count-occurrences (parse-grid input) "XMAS")))

(defn part-2 [_]
  (prn (count-xmas-shapes (parse-grid input))))
