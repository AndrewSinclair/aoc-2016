(ns day-16.core
  (:gen-class))

(def input [0 0 1 1 1 1 0 1 1 1 1 1 0 1 0 0 0])

(defn flip
  [bits]
  (->>
    bits
    reverse
    (map #(-> % inc (mod 2)))))

(defn generate-dragon
  "This is slower"
  [disk-size input]
  (->>
    input
    (iterate #(concat input [0] (flip %)))
    (drop-while #(< (count %) disk-size))
    first
    (take disk-size)))

(defn generate-dragon-recur
  "This is faster"
  [disk-size input]
  (if (>= (count input) disk-size)
    (take disk-size input)
    (recur disk-size (concat input [0] (flip input)))))

(defn checksum
  [curve]
  (->>
    curve
    (iterate
      (fn [bits]
        (->>
          bits 
          (partition 2)
          (map (fn [[x y]] (if (= x y) 1 0))))))
    (filter #(odd? (count %)))
    first))

(defn calc-part-1
  [input]
  (->>
    input
    (generate-dragon 272)
    checksum
    (apply str)))

(defn calc-part-2
  [input]
  (->>
    input
    (generate-dragon-recur 35651584)
    checksum
    (apply str)))

(defn -main
  "Advent of Code - Day 16 - Dragon Fractal"
  [& args]
  (do
    (println (calc-part-1 input))
    (println (calc-part-2 input))))
