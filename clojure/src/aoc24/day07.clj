(ns aoc24.day07
  (:require [clojure.string :as str]))

(def input (str/split-lines (slurp "resources/aoc24/day07.txt")))

;; -- AI! fix this
(defn parse-line [line]
  (let [[test-value & nums] (-> line
                               (str/split #": ")
                               (second)
                               (str/split #"\s+")
                               (cons (first (str/split line #": ")))
                               (map parse-line))]
    {:test-value (first nums) :nums (rest nums)}))

(defn apply-op [op a b]
  (case op
    "+" (+ a b)
    "*" (* a b)))

(defn evaluate [nums ops]
  (loop [result (first nums)
         remaining-nums (rest nums)
         remaining-ops ops]
    (if (empty? remaining-ops)
      result
      (recur (apply-op (first remaining-ops) result (first remaining-nums))
             (rest remaining-nums)
             (rest remaining-ops)))))

(defn generate-op-combinations [num-count]
  (if (= 1 num-count)
    [[]]
    (let [sub-combinations (generate-op-combinations (dec num-count))]
      (concat (map #(cons "+" %) sub-combinations)
              (map #(cons "*" %) sub-combinations)))))

(defn is-valid? [{:keys [test-value nums]}]
  (some (fn [ops] (= test-value (evaluate nums ops)))
        (generate-op-combinations (count nums))))

(defn part-1 [input]
  (->> input
       (map parse-line)
       (filter is-valid?)
       (map :test-value)
       (reduce + 0)))

(def example (str/split-lines "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"))

(map parse-line example)

;; (println (part-1 example))
;; (println (part-1 input))

