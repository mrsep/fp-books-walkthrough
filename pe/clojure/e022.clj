
(defn name-score [s] (reduce + (map #(- (c2i %) 9) (seq s))))
(defn names-read [] (sort (re-seq #"[A-Z]+" (slurp "e022.txt"))))
(defn names-init [] (map vector (iterate inc 1) (names-read)))

(defn solve022 [] (reduce + 
			  (map #(* (first %) (name-score (second %))) 
			       (names-init))))
