(ns day-14.core
  (:require [digest])
  (:gen-class))

(defn have-in-a-row?
  ([n cipher]
    (have-in-a-row? n nil cipher))
  ([n letter cipher]
    (->>
      cipher
      (partition n 1)
      (some #(if (apply = (conj % letter)) (first %))))))

(defn valid-keys
  [candidate-quintuples]
  (let [tripled-letter (->> candidate-quintuples first (have-in-a-row? 3))]
    (if tripled-letter
      (some
         #(have-in-a-row? 5 tripled-letter %)
         (rest candidate-quintuples)))))

(defn calc-part-1
  [input]
  (let [all-hashes (->>
                      (range)
                      (map #(digest/md5 (str input %))))
        six4th-key  (nth
                      (->>
                        all-hashes
                        (partition 1000 1)
                        (filter valid-keys))
                      64)]
    (->>
      all-hashes
      (take-while #(not= six4th-key %))
      count)))

(defn calc-part-2
  [input]
  nil)

(defn -main
  "Advent of Code - Day 14 - Hash codes"
  [& args]
  (let [input  "ahsbgdzn"]
    (do
      (println (calc-part-1 input))
      (println (calc-part-2 input)))))
