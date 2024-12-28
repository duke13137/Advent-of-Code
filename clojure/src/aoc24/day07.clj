(ns aoc24.day07
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[target & nums] (map #(parse-double %) (str/split line #"[: ]+"))]
    {:target target :nums nums}))

(defn apply-op [op a b]
  (case op
    :+ (+ a b)
    :* (* a b)
    :|| (parse-double (str (long a) (long b)))))

(defn calculate [nums ops]
  (reduce (fn [acc [num op]]
            (apply-op op acc num))
          (first nums)
          (map vector (rest nums) ops)))

(defn solve-equation-1 [equation]
  (let [target (:target equation)
        nums (:nums equation)
        num-ops (dec (count nums))]
    (some (fn [ops]
            (= target (calculate nums ops)))
          (for [i (range (int (Math/pow 2 num-ops)))
                :let [ops (map #(if (bit-test i %) :* :+) (range num-ops))]]
            ops))))

(defn solve-equation-2 [equation]
  (let [target (:target equation)
        nums (:nums equation)
        num-ops (dec (count nums))]
    (some (fn [ops]
            (= target (calculate nums ops)))
          (for [i (range (int (Math/pow 3 num-ops)))
                :let [ops (map #(nth [:* :+ :||] (mod (quot i (int (Math/pow 3 %))) 3)) (range num-ops))]]
            ops))))

(defn part-1 [input]
  (let [equations (map parse-line input)]
    (->> equations
         (filter solve-equation-1)
         (map :target)
         (reduce +)
         (long))))

(defn part-2 [input]
  (let [equations (map parse-line input)]
    (->> equations
         (filter solve-equation-2)
         (map :target)
         (reduce +)
         (long))))

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
(println (part-2 example))

(def input (str/split-lines (slurp "resources/aoc24/day07.txt")))

(println (part-1 input))
(println (part-2 input))
