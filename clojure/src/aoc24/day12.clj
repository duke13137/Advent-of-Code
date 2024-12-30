(ns aoc24.day12
  (:require [clojure.string :as str]))

(defn calculate-region
  "Returns a set of all [row col] positions in a connected region of the same plant type"
  [grid start-row start-col plant-type]
  (loop [to-visit #{[start-row start-col]}
         visited #{}]
    (if (empty? to-visit)
      visited
      (let [[row col :as pos] (first to-visit)
            neighbors [[(dec row) col] [(inc row) col]
                       [row (dec col)] [row (inc col)]]
            valid-neighbors (filter (fn [[r c]]
                                      (and (< -1 r (count grid))
                                           (< -1 c (count (first grid)))
                                           (= plant-type (get-in grid [r c]))
                                           (not (visited [r c]))))
                                    neighbors)]
        (recur (into (disj to-visit pos) valid-neighbors)
               (conj visited pos))))))

(defn calculate-region-sides
  "Returns the number of sides for a connected region"
  [grid region]
  (let [rows (count grid)
        cols (count (first grid))]
    (->> region
         (reduce (fn [sides [r c]]
                   (let [neighbors [[(dec r) c] [(inc r) c]
                                    [r (dec c)] [r (inc c)]]]
                     (reduce (fn [s [nr nc]]
                               (if (or (not (< -1 nr rows))
                                       (not (< -1 nc cols))
                                       (not (contains? region [nr nc])))
                                 (inc s)
                                 s))
                             sides
                             neighbors)))
                 0))))

(defn calculate-region-properties
  "Returns [area perimeter] for a connected region"
  [grid row col plant-type]
  (let [region (calculate-region grid row col plant-type)
        area (count region)
        perimeter (reduce + (for [[r c] region
                                  [nr nc] [[(dec r) c] [(inc r) c]
                                           [r (dec c)] [r (inc c)]]
                                  :when (or (not (< -1 nr (count grid)))
                                            (not (< -1 nc (count (first grid))))
                                            (not= plant-type (get-in grid [nr nc])))]
                              1))]
    [area perimeter]))

(defn calculate-total-price-part1 [grid]
  (let [positions (for [row (range (count grid))
                        col (range (count (first grid)))]
                    [row col])]
    (->> positions
         (reduce (fn [{:keys [visited price]} [row col]]
                   (if (visited [row col])
                     {:visited visited :price price}
                     (let [plant-type (get-in grid [row col])
                           [area perimeter] (calculate-region-properties grid row col plant-type)]
                       {:visited (into visited (calculate-region grid row col plant-type))
                        :price (+ price (* area perimeter))})))
                 {:visited #{} :price 0})
         :price)))

(defn calculate-total-price-part2 [grid]
  (let [positions (for [row (range (count grid))
                        col (range (count (first grid)))]
                    [row col])]
    (->> positions
         (reduce (fn [{:keys [visited price]} [row col]]
                   (if (visited [row col])
                     {:visited visited :price price}
                     (let [plant-type (get-in grid [row col])
                           region (calculate-region grid row col plant-type)
                           area (count region)
                           sides (calculate-region-sides grid region)]
                       {:visited (into visited region)
                        :price (+ price (* area sides))})))
                 {:visited #{} :price 0})
         :price)))

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

(def example4 (vec (map vec ["EEEEE" "EXXXX" "EEEEE" "EXXXX" "EEEEE"])))

(def example5 (vec (map vec ["AAAAAA" "AAABBA" "AAABBA" "ABBAAA" "ABBAAA" "AAAAAA"])))

(calculate-total-price-part1 example1) ; 140
(calculate-total-price-part1 example2) ; 772
(calculate-total-price-part1 example3) ; 1930

;; -- AI! update code so that these tests pass
(calculate-total-price-part2 example1) ; 80
(calculate-total-price-part2 example4) ; 236
(calculate-total-price-part2 example5) ; 368


(defn part-1 [input]
  (calculate-total-price-part1 input))

(defn part-2 [input]
  (calculate-total-price-part2 input))

(println (str "Part 1: " (part-1 input)))
(println (str "Part 2: " (part-2 input)))
