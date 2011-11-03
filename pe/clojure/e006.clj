;; Problem 6

;; The sum of the squares of the first ten natural numbers is,
;; 1^(2) + 2^(2) + ... + 10^(2) = 385

;; The square of the sum of the first ten natural numbers is,
;; (1 + 2 + ... + 10)^(2) = 55^(2) = 3025

;; Hence the difference between the sum of the squares of the 
;; first ten natural numbers and the square of the sum is 
;; 3025 − 385 = 2640.

;; Find the difference between the sum of the squares of the 
;; first one hundred natural numbers and the square of the sum.

;; sum of squares
(defn sqr [x] (* x x))
(defn sum-of-squares [n] (reduce + (map sqr (take n (iterate inc 1)))))

(defn square-of-sum [n] (sqr (reduce + (take n (iterate inc 1)))))
(defn square-of-sum-lg [n] (sqr (int (* n (inc n) 0.5))))

(defn solve006 [] (- (sum-of-squares 100) (square-of-sum-lg 100)))
