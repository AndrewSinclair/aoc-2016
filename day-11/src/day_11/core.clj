(ns day-11.core
  (:require [clojure.math.combinatorics :as combos])
  (:gen-class))

(def start-data-1
  [[{:generator :promethium} {:microchip :promethium}]
   [{:generator :cobalt} {:generator :curium} {:generator :plutonium} {:generator :ruthenium}]
   [{:microchip :cobalt} {:microchip :curium} {:microchip :plutonium} {:microchip :ruthenium}]
   []])


(def start-data-2
  [[{:generator :d} {:microchip :e} {:generator :e} {:microchip :d} {:generator :promethium} {:microchip :promethium}]
   [{:generator :cobalt} {:generator :curium} {:generator :plutonium} {:generator :ruthenium}]
   [{:microchip :cobalt} {:microchip :curium} {:microchip :plutonium} {:microchip :ruthenium}]
   []])

(def sort-by-associative-values (comp (partial sort-by second) (partial sort-by first)))

(defn contains-in-list
  [xs x]
  (seq (filter #(= x %) xs)))

(defn set-minus
  [xs ys]
  (->>
    xs
    (filter #(not (contains-in-list (vec ys) %)))))

(defn get-next-moves
  [direction elevator data]
  (concat
    (->>
      (combos/combinations (nth data elevator) 1)
      (map #(vector
              (direction elevator)
              (update-in
                (update-in data [elevator] set-minus %)
                [(direction elevator)] (comp (partial sort-by second) (partial sort-by first) concat) %))))
    (->>
      (combos/combinations (nth data elevator) 2)
      (map #(vector
              (direction elevator)
              (update-in
                (update-in data [elevator] set-minus %)
                [(direction elevator)] (comp (partial sort-by second) (partial sort-by first) concat) %))))))

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
      (filter #(not (contains-in-list (vec generator-types) (:microchip %)))))))

(defn is-floor-invalid-state?
  [floor]
  (let [chips                   (filter :microchip floor)
        generators              (filter :generator floor)
        chips-without-generator (get-chips-wo-generators chips generators)]
    (and (not (or (empty? generators)
                  (empty? chips)
                  (empty? chips-without-generator)))
         (not (some #(contains? (vec generators) (:microchip %)) chips-without-generator)))))

(defn is-invalid-state?
  "on floor n, if there is a chip without matching generator and another generator, then it's invalid.
  If the elevator was on an empty floor, that would be bad too, but the algorithm afaik doesn't run into that..."
  [prev-states [elevator data]]
  (or (contains? prev-states [elevator data])
      (some is-floor-invalid-state? data)))

(defn finished?
  "If the elevator is on floor 3, and if rows 0,1, and 2, are empty, it's done"
  [[elevator data]]
  (and
    (= elevator 3)
    (-> data (subvec 0 3) flatten empty?)))

(defn calc-num-moves
  [init-data init-queue]
  (loop [queue (conj init-queue [0 [0 init-data]])
         prev-states #{}]
    (if (first queue)
      (let [[num-moves curr-move]  (first queue)
            next-moves             (get-all-next-moves curr-move)
            valid-next-moves       (filter (comp not #(is-invalid-state? prev-states %)) next-moves)
            queue                  (reduce #(conj %1 [(inc num-moves) %2]) queue valid-next-moves)
            prev-states            (reduce conj prev-states valid-next-moves)]
        (if (finished? curr-move)
          [num-moves curr-move]
          (recur (pop queue) prev-states))))))

(defn -main
  "Advent of Code - Day 11 - Elevator, Generators and Microchips"
  [& args]
  (let [queue clojure.lang.PersistentQueue/EMPTY]
    (do
      (println (calc-num-moves start-data-1 queue))
      (println (calc-num-moves start-data-2 queue)))))
