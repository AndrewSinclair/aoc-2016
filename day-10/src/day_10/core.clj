(ns day-10.core
  (:require [clojure.string :as string])
  (:gen-class))

(defn parse-input-line
  [input-line]
  (if (.contains input-line "goes to")
    (let [[_ input-val bot] (re-matches #"value (\d+) goes to bot (\d+)" input-line)]
      [:value {(Integer. input-val) {:goes-to-bot [:bot (Integer. bot)]}}])
    (let [[_ robot low-designation low-id high-designation high-id]  (re-matches #"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)" input-line)
          low-heir  [(if (= low-designation "bot") :bot :output) (Integer. low-id)]
          high-heir [(if (= high-designation "bot") :bot :output) (Integer. high-id)]]
      [:robot {(Integer. robot) {:gives-low-to low-heir :gives-high-to high-heir :chips []}}])))

(defn parse-input
  [input]
  (->>
    input
    string/split-lines
    (map parse-input-line)
    (reduce
      #(if (= (first %2) :value)
        (update-in %1 [:values] conj (second %2))
        (update-in %1 [:bots]   conj (second %2)))
      {:values {} :bots {} :outputs []})))

(defn push-all-values
  [queue data]
  (->>
    data
    :values
    (reduce conj queue)))
  
(defn add-chip
  [bot chip]
  (update-in (vec bot) [1 :chips] conj chip))

(defn update-data
  [data next-bot]
  (let [bot-id  (first next-bot)]
    (assoc-in data [:bots bot-id] (second next-bot))))


(defn update-queue
  [queue next-bot]
  (let [new-queue (if (= 2 (-> next-bot second :chips count))
                (conj queue next-bot)
                queue)]
    new-queue))

(defn update-state
  [data queue bot chip]
  (if (= (first bot) :bot)
    (let [next-bot-id (second bot)
          next-bot    [next-bot-id (get-in data [:bots next-bot-id])]
          next-bot    (add-chip next-bot chip)
          data        (update-data data next-bot)
          queue       (update-queue queue next-bot)]
      (vector data queue))
    (let [data  (update-in data [:outputs] conj {:chip chip :output (second bot)})]
      (vector data queue))))

(defn resolve-bot-map
  [data]
  (let [init-queue (-> clojure.lang.PersistentQueue/EMPTY (push-all-values data))]
    (loop [queue init-queue
           data  data]
      (cond
        (:gives-low-to (second (first queue)))
        (let [curr-bot     (first queue)
              curr-bot-id  (first curr-bot)
              low          (:gives-low-to (second curr-bot))
              high         (:gives-high-to (second curr-bot))
              chips        (get-in data [:bots curr-bot-id :chips])
              low-chip     (apply min chips)
              high-chip    (apply max chips)
              [data queue] (update-state data queue low low-chip)
              [data queue] (update-state data queue high high-chip)]
          (recur (pop queue) data))
        (:goes-to-bot (second (first queue)))
        (let [curr-input   (first queue)
              value        (first curr-input)
              bot          (:goes-to-bot (second curr-input))
              [data queue] (update-state data queue bot value)]
          (recur (pop queue) data))
        :else data))))

(defn bot->chips
  [bot]
  (-> bot second :chips))

(defn find-bot-with
  [data chip-ids]
  (->>
    data
    resolve-bot-map
    :bots
    (filter #(= (sort (bot->chips %)) (sort chip-ids)))
    first
    first))

(defn calc-part-1
  [data]
  (find-bot-with data [61 17]))

(defn calc-part-2
  [data]
  (->>
    data
    resolve-bot-map
    :outputs
    (filter #(< (:output %) 3))
    (map :chip)
    (apply *)))

(def data (parse-input (slurp "resources/input.txt")))

(defn -main
  "Advent of Code - Day 10 - Robot Chip Compare"
  [& args]
  (do
    (println (calc-part-1 data))
    (println (calc-part-2 data))))
