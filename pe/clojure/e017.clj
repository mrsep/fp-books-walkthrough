;; CL-USER> (format nil "~R" 1604872898756737459538)

(load-file "eulerlib.clj")

(def digits ["" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])
(def teens  ["ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"])
(def decims ["" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])


(defn num2engl [n] (cond
		     (> n 100) (str 
				(digits (first (number-to-digitseq n))) 
				(cond 
				 (> (mod n 100) 0) "hundredand" (num2engl (mod n 100))
				 (= (mod n 100) 0) "hundred")
		     (= n 100) "onehundred"
		     (< n 100) (cond
				 (> n 19) (str (decims (first (number-to-digitseq n))) (digits (mod n 10)))
				 (> n  9) (teens (mod n 10))
				 (> n  0) (digits (mod n 19)))))

(defn solve017 [] (map num2engl (range 1 1000)))
; ++ one thousand