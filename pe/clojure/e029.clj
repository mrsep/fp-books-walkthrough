;; How many distinct terms are in the sequence generated by a^(b) for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?

(defn sqr [x] (* x x))
(defn abs [x] (if (neg? x) (- x) x))

(defn pow [#^Number x #^Integer n] (loop [s (if (pos? n) x (/ 1 x))
					  j (abs n) 
					  r 1]
				      (if (pos? (quot j 2))
					(recur 
					 (if (pos? j) 
					   (sqr s)
					   s)
					 (quot j 2) 
					 (if (pos? (mod j 2))
					   (* r s)
					   r)
					 )
					(* r s))))

(defn solve029 [n] (apply interleave (map #(map (fn [a] (pow a %)) (range 2 n)) (range 2 n))))
