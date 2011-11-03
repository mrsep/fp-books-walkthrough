;; The palindromic number 595 is interesting because it 
;; can be written as the sum of consecutive squares: 
;; 6^(2) + 7^(2) + 8^(2) + 9^(2) + 10^(2) + 11^(2) + 12^(2).

;; There are exactly eleven palindromes below one-thousand 
;; that can be written as consecutive square sums, and the 
;; sum of these palindromes is 4164. 
;; Note that 1 = 0^(2) + 1^(2) has not been included as 
;; this problem is concerned with the squares of positive integers.

;; Find the sum of all the numbers less than 10^(8) that 
;; are both palindromic and can be written as the sum of consecutive squares.

(load-file "eulerlib.clj")

(defn sqr  [#^Integer n] (* n n))
(defn sqrt [#^Integer n] (Math/sqrt n))
(defn cbrt [#^Integer n] (Math/cbrt n))

; (reduce + (map sqr (range a (+ a k)))))
(defn csumsqr [#^Integer a #^Integer k] (* (inc k) (+ (sqr a) (* k a) (/ (+ (* 2 (sqr k)) k) 6))))

; f(x) = x*(x+1)*(2x+1) = 6*sum[i=1..x](i^2)
(defn f  [#^Number x] (* x (inc x) (inc (* 2 x))))

; f<fs && fs*fsi=id
(defn fsi [#^Integer y] (dec (cbrt (* 0.5 y))))

; fs(6n-f(a)) < m = a+k
(defn calccss [#^Number n #^Number a] 
  (loop [m (max (inc a) (int (fsi (- (* 6 n) (f a))))) 
	 csums (csumsqr a (- m a))]
    (cond 
      (> csums n) 0 ; there exist no m(a): csums of with n = sum[i=a..m](i^2)
      (= csums n) m ; :)
      (< csums n) (recur (inc m) (csumsqr a (- (inc m) a)) ))))

(defn sum-of-sqrs [#^Number n] (let [sqrtn (sqrt n)]
			  (loop [a 1]
			    (if (pos? (calccss n a))
			      [a (calccss n a) n]
			      (if (> a sqrtn)
				[0 0 n]
				(recur (inc a)))))))

;(map #(calccss n %) (range 1 (sqrt n))))
(defn sum-of-sqr? [#^Number n] (let [sqrtn (sqrt n)]
			  (loop [a 1]
			    (if (pos? (calccss n a))
			      true
			      (if (>= a sqrtn)
				false
				(recur (inc a)))))))
  

(defn solve125 [#^Integer n] (filter sum-of-sqr? (palinsk n)))

(defn getak [n] (filter #(pos? %) (map #(calccss n %) (range 1 (sqrt n)))))
