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

(defn solve-equation [equation]
  (let [target (:target equation)
        nums (:nums equation)
        num-ops (dec (count nums))
        ops-combinations (for [i (repeat num-ops [:+ :* :||])]
                           (apply sequence (partition num-ops 1 i)))]
    (some (fn [ops]
            (= target (calculate nums ops)))
          ops-combinations)))

(defn part-1 [input]
  (let [equations (map parse-line input)]
    (->> equations
         (filter solve-equation)
         (map :target)
         (reduce +)
         long)))

(defn part-2 [input]
  (part-1 input))

(def example (str/split-lines "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"))

(println "Part 1 Example:" (part-1 example))
(println "Part 2 Example:" (part-2 example))

(def input (str/split-lines (slurp "resources/aoc24/day07.txt")))

(println "Part 1 Input:" (part-1 input))
(println "Part 2 Input:" (part-2 input))
