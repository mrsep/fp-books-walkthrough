;(ns euler.util
;  (:require clojure.contrib.lazy-seqs))

(defn fibo [] (pmap first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))

(defn recur-fac [#^Number n]
  (letfn [(fac
	   [current n]
	   (if (zero? n)
	     current
	     (recur (* current n) (dec n))))]
    (fac 1 n)))

;; quersumme
(defn c2i [#^Character c] 
  (Character/getNumericValue c))

(defn i2c [#^Integer i]
  (Character/forDigit i 10))

(defn number-to-digitseq [#^Number n] 
  (map c2i (str n)))

(defn update-map [m k e f]
  (let [v (m k)]
    (if v
      (assoc m k (f v))
      (assoc m k e))))


(defn digitseq-to-number [s]
  (Integer/valueOf (apply str (map i2c s))))

(defn quersumme [#^Number n] 
  (reduce + (number-to-digitseq n)))

(defn palindrome? [#^Number n] 
  (= 
   (number-to-digitseq n)
	 (reverse (number-to-digitseq n))))

(defn palins [] (filter palindrome? (iterate inc 1)))

(defn initkern-palins [n] (if (zero? (mod n 2))
			    (range 0 100 11) ; kern der   gerade stelligen Palindrome
			    (range 0 10)))    ; kern der ungerade stelligen Palindrome

(defn tento [#^Integer k] 
  "returns 10^k"
  (bigint (Math/pow 10 k)))

(defn scale [n k] 
  (* n 
    (tento 
      (int (* 0.5 (- k (Math/log10 n)))))))

(defn palin-ext [k] (range (inc (tento (dec k))) (tento k) (inc (tento (dec k)))))
(defn get-palin-ext [ext z] (map #(+ % z) ext))
(defn palinkern-ext [k kern] (let [ext (palin-ext k)]
			       (apply concat (map #(get-palin-ext ext %) (map #(scale % k) kern)))))

; alle palindrome i={2, 1}...{2k=n, 2k+1=n}, +2
(defn palins2 [n] (loop [kern (initkern-palins n) k (- 4 (mod n 2))]
		    (if (= k (+ 2 n))
		      kern
		      (recur (concat kern (palinkern-ext k kern)) (+ k 2)))))

(defn palinsk [n] 
  "gets palindromes up to 10^n, Warning: Zero is two times in"
  (concat (palins2 (dec n)) (palins2 n)))


