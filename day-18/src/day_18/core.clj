(ns day-18.core
  (:gen-class))

(defn text->data
  [text]
  (->>
    text
    (map #(if (= % \^) :trap :safe))))

(defn data->text
  [data]
  (->>
    data
    (map #(if (= % :trap) \^ \.))
    (apply str)))

(def init-data
  (text->data (slurp "resources/input.txt")))

(defn print-rows
  [text-rows]
  (map #(println (data->text %)) text-rows))

(defn next-tile
  "using the if-conditions from the problem,
  but refactored to be simplified"
  [[left centre right]]
  (if (or (= left centre right)
          (= left right))
    :safe
    :trap))

(defn next-row
  [row]
  (->>
    (concat [:safe] row [:safe])
    (partition 3 1)
    (map next-tile)))

(defn calc
  [iterations data]
  (->>
    data
    (iterate next-row)
    (take iterations)
    (map #(->> % (filter (partial = :safe)) count))
    (reduce +)))

(defn -main
  "Advent of Code - Day 18 - Nethack traps with 1D GOL"
  [& args]
  (let [filename "resources/input.txt"
        input    (slurp filename)
        data     (text->data input)]
    (do
      (println (calc 40 data))
      (println (calc 400000 data)))))
