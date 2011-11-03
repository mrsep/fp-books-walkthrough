;; The number, 197, is called a circular prime because all rotations of the i
;; digits: 197, 971, and 719, are themselves prime.

;; There are thirteen such primes below 100: 
;; 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

;; How many circular primes are there below one million?

(load-file "eulerlib.clj")
(load-file "primes.clj")
(use 'clojure.contrib.seq-utils) ; rotations
(use 'clojure.contrib.combinatorics) ; cartesian-product

(defn produce [d] (mapcat 
                    #(apply cartesian-product (repeat % [1 3 7 9]))
                    (range 2 (inc d))))

;; Problem: prime? is tooooooooooooo
(defn f [n] (every? (comp prime? digitseq-to-number) (rotations n)))

(defn solve035 [n] (+ 4 (count (filter f (produce n)))))
