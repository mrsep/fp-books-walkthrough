
(defn sqr [x] (* x x))
(defn sumedges [n] (reduce + (range (sqr n) (sqr (- n 2)) (- (dec n)))))
(defn solve028 [] (reduce + (map sumedges (range 1 1002 2))))