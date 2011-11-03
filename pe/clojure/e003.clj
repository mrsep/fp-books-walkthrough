;; Problem 3

;; The prime factors of 13195 are 5, 7, 13 and 29.

;; What is the largest prime factor of the number 600851475143 ?

;; calculate primes

(load-file "eulerlib.clj")
(load-file "primes.clj")

(defn solve003 [] (prime-factors 600851475143))
