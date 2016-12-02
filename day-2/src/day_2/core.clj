(ns day-2.core
  (require [clojure.string :as str])
  (:gen-class))

(def ^:dynamic *key-pad*)
(def ^:dynamic *left-col*)
(def ^:dynamic *right-col*)
(def ^:dynamic *top-row*)
(def ^:dynamic *bottom-row*)

(defn up
  [curr-index]
  (let [width (count *top-row*)]
    (if (some #(= curr-index %) *top-row*)
      curr-index
      (- curr-index width))))

(defn down
  [curr-index]
  (let [width (count *bottom-row*)]
    (if (some #(= curr-index %) *bottom-row*)
      curr-index
      (+ curr-index width))))

(defn left
  [curr-index]
  (if (some #(= curr-index %) *left-col*)
    curr-index
    (dec curr-index)))

(defn right
  [curr-index]
  (if (some #(= curr-index %) *right-col*)
    curr-index
    (inc curr-index)))

(defn move-finger
  [from direction]
  (direction from))

(defn parse-data
  [input]
  (->>
    input
    str/split-lines
    (map (fn [n] (map #(cond (= % \R) right (= % \L) left (= % \U) up (= % \D) down) n)))))

(defn calc-alg
  [data]
    (->>
      data
      (reductions
        #(reduce move-finger %1 %2)
        5)
      rest
	  (map #(nth *key-pad* %))
      (apply str)))

(def key-pad-1
  [0
   1 2 3
   4 5 6
   7 8 9])

(def key-pad-2
  [0  0  1  0 0
   0  2  3  4 0
   5  6  7  8 9
   0 \A \B \C 0
   0  0 \D  0 0])

(def top-row-1 [1 2 3])
(def top-row-2 [2 6 8 10 14])

(def bottom-row-1 [7 8 9])
(def bottom-row-2 [10 16 22 18 14])

(def left-col-1 [1 4 7])
(def left-col-2 [2 6 10 16 22])

(def right-col-1 [3 6 9])
(def right-col-2 [2 8 14 18 22])

(defn -main
  "Advent of Code Day 2 - Going to the Bathroom"
  [& args]
  (let [filename "resources/input.txt"
        input    (-> filename slurp str/trim)
        data     (parse-data input)]
    (do
	  (binding [*key-pad*    key-pad-1
                *top-row*    top-row-1
                *bottom-row* bottom-row-1
                *left-col*   left-col-1
                *right-col*  right-col-1]
        (println (calc-alg data)))
	  (binding [*key-pad*    key-pad-2
                *top-row*    top-row-2
                *bottom-row* bottom-row-2
                *left-col*   left-col-2
                *right-col*  right-col-2]
        (println (calc-alg data))))))
