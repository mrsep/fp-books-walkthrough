;; If p is the perimeter of a right angle triangle 
;; with integral length sides, {a,b,c}, there are 
;; exactly three solutions for p = 120.

;; {20,48,52}, {24,45,51}, {30,40,50}

;; For which value of p ≤ 1000, is the number of solutions maximised?

(defn getb [a p] (/ (- (* 2 p a) (* p p)) (* 2 (- a p))))
(defn getc [a b p] (- p a b))

(defn sides-of-ra-triangle [p] (filter #(and (pos? %) (integer? %)) (map #(getb % p) (range 1 p))))

(defn solve039 [] (map count (map sides-of-ra-triangle (range 1 1001))))
