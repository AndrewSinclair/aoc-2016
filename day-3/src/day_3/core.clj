(ns day-3.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str])
  (:gen-class))

(defn third
  [xs]
  (nth xs 2))

(defn pivot-data
  [data]
  (->>
    (concat
      (map first data)
      (map second data)
      (map third data))
    (partition 3)))

(defn split-ints
  [line]
  (->>
    (str/split (str/trim line) #"\s+")
    (map #(Integer. %))))

(defn parse-input
  [input]
  (->>
    input
    (str/split-lines)
    (map split-ints)))

(defn triangle-side-combos
  [[A B C]]
  [[A B C]
   [A C B]
   [B C A]])

(defn calc-side-constraint
  [[A B C]]
    (< C (+ A B)))

(defn calc-alg-1
  "Determine the how many triangles are correct based on:
  A + B > C constraint, for arbitrary edge lengths A B and C."
  [data]
  (->>
    data
    (map triangle-side-combos)
    (filter #(every? calc-side-constraint %))
    count))

(defn calc-alg-2
  [data]
  (->>
    data
    pivot-data
    (map triangle-side-combos)
    (filter #(every? calc-side-constraint %))
    count))

(defn -main
  "Advent of Code - Day 3 - Triangle Corridor"
  [& args]
  (let [filename "resources/input.txt"
        input    (slurp filename)
        data     (parse-input input)]
    (do
     (println (calc-alg-1 input))
     (println (calc-alg-2 input)))))
