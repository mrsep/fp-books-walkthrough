;; Problem 2

;; Each new term in the Fibonacci sequence is generated 
;; by adding the previous two terms. By starting with 1 
;; and 2, the first 10 terms will be:
;; 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
;;
;; Find the sum of all the even-valued terms in the 
;; sequence which do not exceed four million.

(load-file "eulerlib.clj")

(defn less-than-4mio? [n] (< n 4000000))

;;
(defn solve-naiv [] (reduce + (filter even? (take-while less-than-4mio? (fibo)))))

;; jedes 3. Folgenglied ist gerade
(defn solve002 [] (reduce + (take-nth 3 (take-while less-than-4mio? (fibo)))))
