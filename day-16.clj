	(defn flip [bits] (map #(if (= % \0) \1 \0) bits))
	(defn generate-dragon [input disk-size] (->> input (iterate #(apply str % "0" (flip (reverse %)))) (drop-while #(< (count %) disk-size)) first (take disk-size)))

	(defn checksum [xs] (->> (iterate (fn [bits] (->> bits (partition 2) (map #(if (= (first %) (second %)) \1 \0)) (apply str))) xs) (filter #(odd? (count %))) first))
	
	(def input "00111101111101000")
	(checksum (generate-dragon input 272))
	(checksum (generate-dragon input 35651584))
