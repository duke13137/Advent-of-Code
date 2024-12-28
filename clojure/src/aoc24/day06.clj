(ns aoc24.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        grid (mapv vec lines)
        guard-pos (first (for [y (range (count grid))
                                 x (range (count (grid y)))
                                 :when (= \^ (get-in grid [y x]))]
                             [y x]))]
    [grid guard-pos :up]))

(defn turn-right [direction]
  (case direction
    :up :right
    :right :down
    :down :left
    :left :up))

(defn move-forward [[y x] direction]
  (case direction
    :up [(dec y) x]
    :right [y (inc x)]
    :down [(inc y) x]
    :left [y (dec x)]))

(defn is-obstacle? [grid [y x]]
  (or (not (and (>= y 0) (< y (count grid))
                (>= x 0) (< x (count (grid y)))))
      (= \# (get-in grid [y x] :oob))))

(defn simulate-guard [grid initial-position initial-direction]
  (loop [position initial-position
         direction initial-direction
         visited #{[initial-position initial-direction]}]
    (let [next-position (move-forward position direction)]
      (if (or (is-obstacle? grid next-position)
              (visited [next-position direction]))
        (let [new-direction (turn-right direction)
              next-state [position new-direction]]
          (if (visited next-state)
            (count (set (map first visited)))
            (recur position new-direction (conj visited next-state))))
        (recur next-position direction (conj visited [next-position direction]))))))

(def input (slurp "resources/aoc24/day06.txt"))

(defn part-1
  "Run with bb -x aoc24.day06/part-1"
  [input]
  (let [[map initial-position initial-direction] (parse-input input)
        visited-count (simulate-guard map initial-position initial-direction)]
    visited-count))

;; (defn part-2
;;   "Run with bb -x aoc24.day02/part-2"
;;   [_]
;;   (->> input
;;        (partition-by nil?)
;;        (take-nth 2)
;;        (map #(apply + %))
;;        (sort-by -)
;;        (take 3)
;;        (apply +)
;;        prn))


(def example "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(part-1 example)
;; (part-2 example)

;; (part-1 input)
;; (part-2 input)
