;; Problem 16
;; What is the sum of the digits of the number 2^(1000)

(load-file "eulerlib.clj")

(defn solve016 [] (quersumme (last (take 1000 (iterate #(* 2 %) 2)))))