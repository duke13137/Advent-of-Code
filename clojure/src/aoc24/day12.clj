(ns aoc24.day12
  (:require [clojure.string :as str]))

#_#_(defn calculate-region-properties [grid start-row start-col plant-type visited]
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


(defn calculate-total-price [grid]
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
