(ns my-tjoc.c6)

;;; part 3: Functional Programming
;;; chapter 6 - Being lazy and set in your ways

;; Immutability, Persistency, Laziness

; produce a lazy seq with the lazy-seq macro 

(defn triangle [n]
  (/ (* n (+ n 1)) 2))

; lazy sequence of triangle numbers
(def tri-nums (map triangle (iterate inc 1)))
; allows more interesting queries

;; call-by-need, or explicit lazyness with delay and force
;; building blocks of lazyness

(defn defer-expensive [cheap expensive]
  (if-let [good-enough (force cheap)]
    good-enough
    (force expensive)))

(comment
  (defer-expensive
    (delay :cheap)
    (delay (do (Thread/sleep 5000) :expensive)))

  (defer-expensive
    (delay false)
    (delay (do (Thread/sleep 5000) :expensive))))
; delay is memoized

; head strict evaluation
(defn inf-triangles [n]
  {:head (triangle n)
   :tail (delay (inf-triangles (inc n)))})

(defn head [l] (:head l))
(defn tail [l] (force (:tail l)))

(def tri-nums2 (inf-triangles 1))

;; lazy quicksort


nil
