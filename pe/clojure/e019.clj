;; first day of year 1900 was a Monday

(def days ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])
(def mons-normal [0 31 28 31 30 31 30 31 31 30 31 30 31])
(def mons-schalt [0 31 29 31 30 31 30 31 31 30 31 30 31])

(defn centuryday [n] (days (mod n 7)))

;; 01.01.1901 - 31.12.2000

(defn schalt? [y] (if (zero? (mod y 100))
		   (zero? (mod y 400))
		   (zero? (mod y 4))))
		   
(defn days-per-month-in-year [y] (if (schalt? y)
				   mons-schalt
				   mons-normal))

;; since 01.01.1900
(defn nr-ofthe-fst-day-in-year [y] (assert (>= y 1900)) 
  (loop [yn 1900 days 0]
    (if (= yn y)
      (inc days)
      (recur 
       (inc yn)
       (+ days (reduce + (days-per-month-in-year yn)))))))

(defn get-nr-of-fstdays-in-my [y]
  (let [year (days-per-month-in-year y)]
    (loop [data [(nr-ofthe-fst-day-in-year y)] m 1]
      (if (= 12 m)
	data
	(recur (conj data (+ (peek data) (year m))) (inc m))))))

(defn fst-days-in-y [y]
  "returns the first days for every month in year y"
  (map centuryday (get-nr-of-fstdays-in-my y)))

(defn solve019 [] (count (filter #(= "Sunday" %) (apply concat (map fst-days-in-y (range 1901 2001))))))

; or 1200/7
