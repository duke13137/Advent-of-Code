(ns aoc24.day07
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[target & nums] (map #(parse-double %) (str/split line #"[: ]+"))]
    {:target target :nums nums}))

(defn apply-op [op a b]
  (case op
    :+ (+ a b)
    :* (* a b)))

(defn calculate [nums ops]
  (reduce (fn [acc [num op]]
            (apply-op op acc num))
          (first nums)
          (map vector (rest nums) ops)))

(defn solve-equation [equation]
  (let [target (:target equation)
        nums (:nums equation)
        num-ops (dec (count nums))]
    (some (fn [ops]
            (= target (calculate nums ops)))
          (for [i (range (int (Math/pow 2 num-ops)))
                :let [ops (map #(if (bit-test i %) :* :+) (range num-ops))]]
            ops))))

(defn part-1 [input]
  (let [equations (map parse-line input)]
    (doseq [eq equations]
      (println eq))
    (->> equations
         (filter solve-equation)
         (map :target)
         (reduce +))))


(def input (str/split-lines (slurp "resources/aoc24/day07.txt")))

(def example (str/split-lines "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"))


(println (part-1 example))
(println (part-1 input))
