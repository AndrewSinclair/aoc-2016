(ns day-11.core
  (:gen-class))

(def start-data
  [[{:generator :promethium} {:microchip :promethium}]
   [{:generator :cobalt} {:generator :curium} {:generator :ruthenium} {:generator plutonium}]
   [{:microchip :cobalt} {:microchip :curium} {:microchip :ruthenium} {:microchip plutonium}]
   []])

(def test-data
  [[{:microchip :hydrogen} {:microchip :lithium}]
   [{:generator :hydrogen}]
   [{:generator :lithium}]
   []])

(defn get-all-next-moves
  "move the elevator and (1 or 2 things) up or down
  Don't forget you can't move below 0 or above 3"
  [elevator data])

(defn is-invalid-state?
  "on floor n, if there is a chip without matching generator and another generator, then it's invalid"
  [data]
  )

(defn calc-part-1
  [init-data]
  (loop [elevator 0
         data init-data
         state-stack []]
    (let [next-moves (get-all-next-moves elevator data)
         valid-next-moves (filter (not is-invalid-state?) next-moves)
         next-move   (first valid-next-moves)
        ;; actually, use a queue; state-stack (reduce conj state-stack valid-next-moves)
         ]
      (if ))))

(defn -main
  "Advent of Code - Day 11 - Elevator, Generators and Microchips"
  [& args]
  (do
    (println calc-part-1 data)
    (println calc-part-2 data)))
