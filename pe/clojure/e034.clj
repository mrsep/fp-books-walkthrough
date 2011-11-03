;; 145 is a curious number, as 
;; 1! + 4! + 5! = 1 + 24 + 120 = 145.

;; Find the sum of all numbers which are 
;; equal to the sum of the factorial of their digits.

;; Note: as 1! = 1 and 2! = 2 are not sums they are not included.

(load-file "eulerlib.clj")

(defn curious? [n] (= n (reduce + (map recur-fac (number-to-digitseq n)))))

(defn solve034 [n] (reduce + (filter curious? (range 10 n))))
