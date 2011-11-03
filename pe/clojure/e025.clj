;; Problem 25

;; What is the first term in the Fibonacci sequence to contain 1000 digits?

(load-file "eulerlib.clj")

(defn digits-count [x] (count (number-to-digitseq x)))

(defn solve025 [] (count (take-while #(< (digits-count %) 1000) (fibo))))
