(def input (clojure.string/split-lines (slurp "aoc24/day06.txt")))

(defn parse-grid [input]
  ; ... Convert input lines into a 2D grid representation ...
)

(defn find-start [grid]
  ; ... Find the guard's starting position (row, col) in the grid ...
)

(defn is-valid? [grid row col]
  ; ... Check if (row, col) is within the grid bounds ...
)

(defn is-obstacle? [grid row col]
  ; ... Check if (row, col) is an obstacle in the grid ...
)

(defn turn-right [direction]
  ; ... Return the new direction after turning right ...
)

(defn next-position [row col direction]
  ; ... Calculate the next position based on current position and direction ...
)

(defn simulate-guard [grid start-row start-col]
  ; ... Simulate the guard's movement and return the set of visited positions ...
)

(defn part-1 [input]
  (let [grid (parse-grid input)
        [start-row start-col] (find-start grid)
        visited (simulate-guard grid start-row start-col)]
    (count visited)))

(defn part-2 [input]
  ; ... (Not defined in this part of the problem) ...
)

(def example (clojure.string/split-lines "...")) ; Replace ... with the example input

(part-1 example)
;(part-2 example) ; (Not applicable yet)
