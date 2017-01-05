(ns day-20.core
  (:require [clojure.string :as string])
  (:gen-class))

(def input (slurp "resources/input.txt"))

(defn text->data
  [text]
  (->>
    text
    string/split-lines
    (map #(string/split % #"-"))
    (map (fn [[to from]] [(Long. to) (Long. from)]))))

(defn data->fns
  [data]
  (->>
    data
    (map (fn [[to from]] #(if (and (<= to %) (>= from %)) %)))))

(defn in-some-range?
  [fns n]
  (some #(% n) fns))

(defn calc-part-1
  "This is very slow. Instead of looking through all data each time (up to the point that it finds the val),
  try ``minimizing'' the data into the smallest amount of ranges needed. The other thing we can try 
  is to go through the data once, and update a range filtering out anything in the numbers.
  Then use this lazy-list of numbers with `some` to see if the number is in it. Or just count the list
  for part 2."
  [text]
  (let [data          (text->data text)
        ip-range-fns  (data->fns data)]
    (->>
      (range)
      (filter (comp not (partial in-some-range? ip-range-fns)))
      first)))

(defn calc-part-2
  [text]
  nil)

(defn -main
  "Advent of Code - Day 20 - IP Ranges"
  [& args]
  (do
    (println (calc-part-1 input))
    (println (calc-part-2 input))))
