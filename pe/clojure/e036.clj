;; Problem 36

;; The decimal number, 585 = 1001001001_(2) (binary), 
;; is palindromic in both bases.

;; Find the sum of all numbers, less than one million, 
;; which are palindromic in base 10 and base 2.
;; (Please note that the palindromic number, in either base, may not include leading zeros.)

(load-file "eulerlib.clj")

(defn to-radix [#^Integer n r] (Integer/toString n r))

(defn palindrome-in-radix-2? [#^Integer n] (=  
				  (seq (to-radix n 2))
				  (reverse (to-radix n 2))))

(defn problist [] (take-while #(< % 1000000) (palins)))

(defn solve036 [] (reduce + (filter palindrome-in-radix-2? (problist))))