;; Problem 4

;; A palindromic number reads the same both ways. 
;; The largest palindrome made from the product of 
;; two 2-digit numbers is 9009 = 91 × 99.

;; Find the largest palindrome made from the product of two 3-digit numbers.

(load-file "eulerlib.clj")

(defn problist [] (drop-while #(< % 10000) 
		  (take-while #(< % 1000000) (palins))))

(defn in-range? [n] (and
		     (<= n  999)
		     (>= n  100)))

(defn teiler? [n] 
  (some
    #(and 
       (= 0 (mod n %)) 
       (in-range? (/ n %)))   
		(range 100 999)))

(defn solve004 [] (last (filter teiler? (problist))))
