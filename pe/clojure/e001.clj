;; Euler Problem 1

;; If we list all the natural numbers below 10 
;; that are multiples of 3 or 5, we get 3, 5, 6 
;; and 9. The sum of these multiples is 23.

;; Find the sum of all the multiples of 3 or 5 below 1000.


;; generalizable!!
(defn m35? [x] 
  (or 
   (zero? (mod x 3)) 
   (zero? (mod x 5))))

;; brut-force
(defn solve001 [] (reduce + (filter m35? (range 1 1000))))
