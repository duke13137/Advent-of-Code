(ns aoc24.day12
  (:require [clojure.string :as str]))

;; -- AI! refactor this function using clojure idioms e.g. transducer but keep the logic the same!
(defn calculate-region-properties [grid start-row start-col plant-type visited]
  (let [rows (count grid)
        cols (count (first grid))
        area (atom 0)
        perimeter (atom 0)
        queue (java.util.LinkedList. [(list start-row start-col)])]
    (swap! visited conj [start-row start-col])

    (while (seq queue)
      (let [[row col] (.removeFirst queue)]
        (swap! area inc)

        (doseq [[dr dc] [[0 1] [0 -1] [1 0] [-1 0]]]
          (let [new-row (+ row dr)
                new-col (+ col dc)]

            (cond
              (or (< new-row 0) (>= new-row rows) (< new-col 0) (>= new-col cols))
              (swap! perimeter inc)

              (and (= (get-in grid [new-row new-col]) plant-type)
                   (not (contains? @visited [new-row new-col])))
              (do
                (.addLast queue (list new-row new-col))
                (swap! visited conj [new-row new-col]))

              (not= (get-in grid [new-row new-col]) plant-type)
              (swap! perimeter inc))))))
    [@area @perimeter]))

(defn calculate-total-price [grid]
  (let [rows (count grid)
        cols (count (first grid))
        visited (atom #{})
        total-price (atom 0)]
    (doseq [row (range rows)
            col (range cols)]
      (when (not (contains? @visited [row col]))
        (let [plant-type (get-in grid [row col])
              [area perimeter] (calculate-region-properties grid row col plant-type visited)]
          (swap! total-price + (* area perimeter)))))
    @total-price))

(def input (vec (map vec (str/split-lines (slurp "resources/aoc24/day12.txt")))))

(def example1 (vec (map vec ["AAAA" "BBCD" "BBCC" "EEEC"])))

(def example2 (vec (map vec ["OOOOO" "OXOXO" "OOOOO" "OXOXO" "OOOOO"])))

(def example3 (vec (map vec ["RRRRIICCFF"
                             "RRRRIICCCF"
                             "VVRRRCCFFF"
                             "VVRCCCJFFF"
                             "VVVVCJJCFE"
                             "VVIVCCJJEE"
                             "VVIIICJJEE"
                             "MIIIIIJJEE"
                             "MIIISIJEEE"
                             "MMMISSJEEE"])))

(println (str "Example 1 Total Price: " (calculate-total-price example1)))
(println (str "Example 2 Total Price: " (calculate-total-price example2)))
(println (str "Example 3 Total Price: " (calculate-total-price example3)))

(defn part-1 [input]
  (calculate-total-price input))

(defn part-2 [input]
  (calculate-total-price input))

(println (str "Part 1: " (part-1 input)))
(println (str "Part 2: " (part-2 input)))
