;; Problem 13

;; Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.

(defn solve013 [] (reduce + 
			  (map #(bigint (apply str %)) 
			       (partition 50 
					  (filter #(Character/isDigit %) 
						  (seq (slurp "e013.txt")))))))
