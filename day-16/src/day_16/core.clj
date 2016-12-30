(ns day-16.core
  (:gen-class))

(def input "00101000101111010")

(defn flip
  [bits]
  (->>
    bits
    reverse
    (map #(if (= \0 %) \1 \0))
    (concat bits [\0])))

(defn generate-dragon
  [disk-size input]
  (->>
    input
    (iterate #(flip %))
    (drop-while #(< (count %) disk-size))
    first
    (take disk-size)))

(defn generate-dragon-recur
  [disk-size input]
  (if (>= (count input) disk-size)
    (take disk-size input)
    (recur disk-size (flip input))))

(defn checksum
  [xs]
  (->>
    xs
    (iterate
      (fn [bits]
        (->>
          bits 
          (partition 2)
          (map #(if (= (first %) (second %)) \1 \0)))))
    (filter #(odd? (count %)))
    first))

(defn calc-part-1
  [input]
  (->>
    input
    (generate-dragon-recur 272)
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
