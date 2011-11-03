(defn powe [x] (loop [a 1 n x] 
		(if (zero? n) 
		  a
		  (recur (* a x) (dec n)))))

(defn solve048 [] (str (reduce + (map powe (range 1 1001)))))