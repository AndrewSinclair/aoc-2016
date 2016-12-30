(ns day-19.core
  (:gen-class))

(defn take-left
  [acc xs]
  ())


(defn calc-part-1
  [input]
  (->>
    (range input)
    set
    (reduce take-left)))

(defn calc-part-2
  [input]
  nil)

(defn -main
  "Advent of Code - Day 19 - Josephus Elf"
  [& args]
  (let [input 3004953]
    (do
      (println (calc-part-1 3004953))
      (println (calc-part-2 3004953)))))
