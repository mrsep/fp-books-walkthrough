(load-file "eulerlib.clj")

;(defn divisors [n] "n=k*p" => k,p are divisors, only to sqrt(n))
;alternative: by the prime-factor-decomposition

(defn divisors-brut ""
  [#^Number n]
  (filter #(= 0 (mod n %)) 	  
          (range (quot n 2) 0 -1)))

(defn divisors-prime [#^Number n]
  (filter #(= 0 (mod n %))
          (take-while #(>= (quot n 2) %) primes)))

(defn divisors-prime-to [n to]
  (filter #(= 0 (mod n %))
          (take to primes)))

(defn next-mult [coll current max] 
  (loop [next (+ 2 current)]
    (if (or 
          (coll next)
					(> next max))
			next
			(recur (+ 2 next)))))

(defn multiples [k] (iterate #(+ k %) (* 2 k)))

(defn mflush-set [pset current n] (apply disj pset 
				     (take-while #(< % n) (multiples current))))

;; better with a lazy sequence!!!
(defn primes-to-set [n] (loop 
			[pset (mflush-set (set (range 2 n)) 2 n) current 3]
		      (if (> (* 2 current) n)
			pset
			(recur 
			 (mflush-set pset current n)
			 (next-mult pset current n)))))

;; try with a sequence
(defn mflush [pseq current n] (apply disj pseq 
				     (take-while #(< % n) (multiples current))))

(defn primes-to [n] (loop 
			[pseq (mflush (range 2 n) 2 n) current 3]
		      (if (> (* 2 current) n)
			pseq
			(recur 
			 (mflush pseq current n)
			 (next-mult pseq current n)))))

(defn prime? [n] (if (> n 1)
		   (not-any? #(zero? (mod n %)) 
			     (primes-to-set (inc (bigint (* n 0.5)))))
		   false))

;; given number to calc all prime factors of: n
;; find one prime factor, could be small! p1
;; recursive call prime with n/p1
(defn prime-factors [n]
  "calculates all prime factors of a given number n and returns them in a set"
  (loop [curnum n 
         fcol #{}
         curfac (first (divisors-prime n))]
    (if curfac
      (recur (/ curnum curfac)
             (conj fcol curfac)
             (first (divisors-prime (/ curnum curfac))))
      (conj fcol curnum))))

(defn prime-factors-decomposition [n]
  "calculates all prime factors of a given number n and returns them in a map 
   as the key, the values are the exponents of the factors"
  (loop [curnum n 
         fcol (hash-map)
         curfac (first (divisors-prime n))]
    (if curfac
      (recur (quot curnum curfac)
             (update-map fcol curfac 1 inc)
             (first (divisors-prime (quot curnum curfac))))
      (update-map fcol curnum 1 inc))))

(defn divisors-count [n]
  "calculates the amount of all divisors of n"
  (reduce * 1 (map (comp inc val) (prime-factors-decomposition n))))

;; --> ggt, kgv, ...
