(ns day-15.core
  (:gen-class))

(def xss
  [(map #(-> % (* 19) (+ 17)) (range))
   (map #(-> % (* 13) (+  2)) (range))
   (map #(-> % (* 5)  (+  1)) (range))
   (map #(-> % (* 17) (+  8)) (range))
   (map #(-> % (* 3)        ) (range))
   (map #(-> % (* 7)  (+  2)) (range))])

(defn contains-in-seq?
  "Assumes xs is numbers in order, otherwise it works with infinite lazy seqs which is the point"
  [x xs]
  (= x (first (drop-while #(< % x) xs))))

(defn sieve
  [acc xs]
  (->>
    acc
    (filter #(contains-in-seq? % xs))))

(defn calc-part-1
  []
  (->>
    xss
    (reduce sieve)
    first))

(defn calc-part-2
  []
  nil)

(defn -main
  "Advent Of Code - Day 15 - Rotary Disc Kinetic Sculpture"
  [& args]
  (do
    (println (calc-part-1))
    (println (calc-part-2))))
