;; A perfect number is a number for which the sum of its proper divisors is 
;; exactly equal to the number. For example, the sum of the proper divisors 
;; of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect 
;; number.

;; A number n is called deficient if the sum of its proper divisors is less 
;; than n and it is called abundant if this sum exceeds n.

;; As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the 
;; smallest number that can be written as the sum of two abundant numbers 
;; is 24. By mathematical analysis, it can be shown that all integers 
;; greater than 28123 can be written as the sum of two abundant numbers. 
;; However, this upper limit cannot be reduced any further by analysis even 
;; though it is known that the greatest number that cannot be expressed as 
;; the sum of two abundant numbers is less than this limit.

;; Find the sum of all the positive integers which cannot be written as the 
;; sum of two abundant numbers.

(load-file "primes.clj")

(defn fail-to-perfect
	"calculates the sum of n's proper divisors and sums them, and 
         subtracts it from n. Therefor:
         result < 0: deficient
         result = 0: perfect
         result > 0: abundant"
        [n]
	(- (reduce + 0 (divisors-brut n)) n))

(defn perfect? [n]
	(zero? (fail-to-perfect n)))

(defn deficient? [n]
	(neg? (fail-to-perfect n)))

(defn abundant? [n]
	(pos? (fail-to-perfect n)))

(defn abundants-to [n]
	(filter abundant? (range 2 n)))

(def limit 28124)

(def abd 
  (filter abundant? (range 0 limit)))
;; or memoize it as function
(def inq (vec 
           (map-indexed (fn [idx itm] [idx (abundant? itm)]) (range 0 limit))))

(defn sum-of-this-abundant? [n a]
  (if (> n a)
    (second (inq (- n a)))))

(defn sum-of-abundant [n] 
  (some #(sum-of-this-abundant? n %) abd))

(defn not-sum-of-abundants [lim]
  (filter #(not (sum-of-abundant %)) (range 1 lim)))

(defn solve023 [] (reduce + 0 (not-sum-of-abundants limit)))


