(ns day-11.core
  (:require [clojure.math.combinatorics :as combos])
  (:use [clojure.pprint])
  (:gen-class))

(def start-data
  [[{:generator :promethium} {:microchip :promethium}]
   [{:generator :cobalt} {:generator :curium} {:generator :ruthenium} {:generator :plutonium}]
   [{:microchip :cobalt} {:microchip :curium} {:microchip :ruthenium} {:microchip :plutonium}]
   []])

(def test-data
  [[{:microchip :hydrogen} {:microchip :lithium}]
   [{:generator :hydrogen}]
   [{:generator :lithium}]
   []])

(def invalid-data
  [[{:microchip :hydrogen} {:generator :lithium}]
  []
  []
  []])

(def valid-data
  [[{:microchip :hydrogen} {:generator :hydrogen}]
  []
  []
  []])

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn contains
  [xs x]
  (not (zero? (count (filter #(= % x) xs)))))

(defn set-minus
  [xs ys]
  (->>
    xs
    (filter #(not (contains ys %)))))

(defn get-next-moves
  [direction elevator data]
  (concat
    (->>
      (combos/combinations (nth data elevator) 1)
      (map #(vector
            (direction elevator)
            (update-in
              (update-in data [elevator] set-minus %)
              [(direction elevator)] concat %))))
    (->>
      (combos/combinations (nth data elevator) 2)
      (map #(vector
              (direction elevator)
              (update-in
                (update-in data [elevator] set-minus %)
                [(direction elevator)] concat %))))))

(defn get-all-next-moves
  "move the elevator and (1 or 2 things) up or down
  Don't forget you can't move below 0 or above 3"
  [[elevator data]]
  (cond
    (= 3 elevator) (get-next-moves dec elevator data)
    (= 0 elevator) (get-next-moves inc elevator data)
    :else (concat [] (get-next-moves inc elevator data)
                     (get-next-moves dec elevator data))))

(defn get-chips-wo-generators
  [chips generators]
  (let [generator-types (map :generator generators)]
    (->>
      chips
      (filter #(not (contains generator-types (:microchip %)))))))

(defn is-floor-invalid-state?
  [floor]
  (let [chips                   (filter :microchip floor)
        generators              (filter :generator floor)
        chips-without-generator (get-chips-wo-generators chips generators)]
    (and (not (or (zero? (count generators))
                  (zero? (count chips))
                  (zero? (count chips-without-generator))))
         (not (some #(contains generators (:microchip %)) chips-without-generator)))))

(defn is-invalid-state?
  "on floor n, if there is a chip without matching generator and another generator, then it's invalid.
  If the elevator was on an empty floor, that would be bad too, but the algorithm afaik doesn't run into that..."
  [[_ data]]
  (some is-floor-invalid-state? data))

(defn finished?
  "If the elevator is on floor 3, and if rows 0,1, and 2, are empty, it's done"
  [[elevator data]]
  (and
    (= elevator 3)
    (-> data (subvec 0 3) flatten count zero?)))

(defn calc-part-1
  [init-data init-queue max-loops]
  (loop [run-away max-loops
    queue (conj init-queue [0 init-data])]
    (if (first queue)
      (let [curr-move        (first queue)
            next-moves       (get-all-next-moves curr-move)
            valid-next-moves (filter (comp not is-invalid-state?) next-moves)
            queue            (reduce conj queue valid-next-moves)]
        (do (pprint [curr-move run-away])
        (if (or (zero? run-away) (finished? curr-move))
          curr-move
          (recur (dec run-away) (pop queue))))))))


(defn calc-part-2
  [init-data init-queue]
  nil)


(defn -main
  "Advent of Code - Day 11 - Elevator, Generators and Microchips"
  [& args]
  (do
    (println (calc-part-1 start-data empty-queue))
    (println (calc-part-2 start-data empty-queue))))
