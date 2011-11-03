;; Problem 9

;; A Pythagorean triplet is a set of three natural 
;; numbers, a < b < c, for which,
;; a^(2) + b^(2) = c^(2)

;; For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).

;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;; Find the product abc.

(defn sqr [n] (* n n))
(defn sqrt [n] (Math/sqrt n))
(defn sumsqr [a b] (+ (sqr a) (sqr b)))
(defn is-square? [#^Integer n] (= n (sqr (int (sqrt n)))))

(defn pythagorean2? [a b] (is-square? (sumsqr a b)))

(defn pythagorean3? [a b c] (zero? 
			    (+ (sqr a) (sqr b) (- (sqr c)))))

;; a+b+c = 1000 => 1 < a,b,c < 998

;; hold a, search b: a,b,c pythagorean.

(defn get-pythagorean [a] (filter #(and 
				    (pythagorean2? a %)
				    (= 1000 (+ a % (int (sqrt (sumsqr a %))))))
				  (range 1 998)))

(defn solve009 [] (filter #(not (empty? %)) (map get-pythagorean (range 1 998))))
