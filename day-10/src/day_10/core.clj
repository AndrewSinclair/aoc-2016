(ns day-10.core
  (:require [clojure.string :as string])
  (:gen-class))

(defn parse-input-line
  [input-line]
  (if (.contains input-line "goes to")
    (let [[_ input-val bot] (re-matches #"value (\d+) goes to bot (\d+)" input-line)]
      [:value {(Integer. input-val) {:goes-to-bot bot}}])
    (let [[_ robot low-designation low-id high-designation high-id]  (re-matches #"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)" input-line)
          low-heir  [(if (= low-designation "bot") :bot :output) (Integer. low-id)]
          high-heir [(if (= high-designation "bot") :bot :output) (Integer. high-id)]]
      [:robot {robot {:gives-low-to low-heir :gives-high-to high-heir :chips []}}])))

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
      {:values {} :bots {}})))

(defn push-all-values
  [queue data]
  (->>
    data
    :values
    (reduce conj queue)))
  
(defn add-chip
  [bot chip]
  (update-in bot [1 :chips] conj chip))

(defn update-data
  ([data next-bot]
  (let [bot-id  (first next-bot)]
    (assoc-in data [:bots bot-id] (second next-bot))))
  ([data low-bot high-bot]
  (let [low-bot-id  (first low-bot)
        high-bot-id (first high-bot)]
  (->
    data
    (assoc-in [:bots low-bot-id] (second low-bot))
    (assoc-in [:bots high-bot-id] (second high-bot))))))

(defn update-queue
  ([queue low-bot high-bot]
  (let [
    queue (if (= 2 (count (:chips (second low-bot)))) (conj queue low-bot) queue)
	queue (if (= 2 (count (:chips (second high-bot)))) (conj queue high-bot) queue)]
	queue))
  ([queue next-bot]
  (let [queue (if (= 2 (count (:chips (second next-bot)))) (conj queue next-bot) queue)]
	queue)))


(defn resolve-bot-map
  "enter all the values first, then go through it. When a bot has two, enter them too."
  [data]
  (let [queue (-> clojure.lang.PersistentQueue/EMPTY (push-all-values data))]
    (loop [queue queue
           data  data]
      (cond
        (:gives-low-to (second (first queue)))
	    (let [curr-bot         (first queue)
              curr-bot-id      (first curr-bot)
              low-bot-id       (:gives-low-to  curr-bot)
              high-bot-id      (:gives-high-to curr-bot)
              chips            (get-in data [:bots curr-bot-id :chips])
              low-chip         (apply min chips)
              high-chip        (apply max chips)
              next-low-bot     [low-bot-id (get-in data [:bots low-bot-id])]
              next-high-bot    [high-bot-id (get-in data [:bots high-bot-id])]
              updated-low-bot  (add-chip next-low-bot low-chip)
              updated-high-bot (add-chip next-high-bot high-chip)
              data             (update-data data updated-low-bot updated-high-bot)
              queue            (update-queue (pop queue) updated-low-bot updated-high-bot)]
          (recur queue data))
        (:goes-to-bot (second (first queue)))
        (let [chip-id      (first (first queue))
              next-bot-id  (:goes-to-bot (second (first queue)))
              next-bot     [next-bot-id (get-in data [:bots next-bot-id])]
              updated-bot  (add-chip next-bot chip-id)
              data         (update-data data updated-bot)
              queue        (update-queue (pop queue) updated-bot)]
          (recur queue data))
        :else data))))

(defn find-bot-with
  [data chip-ids]
  (->>
    data
    resolve-bot-map
    (filter #(= (get-in % [:bots 1 :chips]) ["16" "67"]))))

(defn calc-part-1
  [data]
  (find-bot-with data nil))

(defn calc-part-2
  [data]
  nil)

(def data (parse-input (slurp "resources/input.txt")))

(defn -main
  "Advent of Code - Day 10 - Robot Chip Compare"
  [& args]
  (do
    (println (calc-part-1 data))
    (println (calc-part-2 data))))
