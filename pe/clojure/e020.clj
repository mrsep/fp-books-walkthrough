;; Find the sum of the digits in the number 100!

(load-file "eulerlib.clj")

(defn solve020 [#^Number n] 
  (quersumme (recur-fac n)))
