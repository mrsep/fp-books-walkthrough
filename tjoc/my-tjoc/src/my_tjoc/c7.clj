(ns my-tjoc.c7)

;;; chapter 7 - Functional Programming

;; functions are first class citizens
;; create, store, pass and return them

; collections can act as functions
(map [:chthon :phthor :beowulf :grendel] '(1 3))

; composition of functions with comp

(def add4 (comp inc inc inc inc))

(defn fnth [f n] 
  (apply comp (take n (repeat f))))

(comment
  (add4 1)
  ((fnth inc 4) 0))

; partial functions (which is not currying)
(def incsum (partial + 1) )

; also: functions can act as data, and they are data

(defn join
  {:test (fn []
           (assert
            (= (join "," [1 2 3]) "1,3,3")))}
  [sep s]
  (apply str (interpose sep s)))

(comment
  (use '[clojure.test :as t])
  (t/run-tests)
)

; Higher order functions



;; lexical closures
(def times-two
  (let [x 2]
    (fn [y] (* x y))))

(def add-and-get
  (let [ai (java.util.concurrent.atomic.AtomicInteger.)]
    (fn [y] (.addAndGet ai y))))

(defn times-three [n]
  (fn [y] (* n y)))

;; a bot - sharing closure context

(def bearings [{:x  0, :y  1}   ; north
               {:x  1, :y  0}   ; east
               {:x  0, :y -1}   ; south
               {:x -1, :y  0}]) ; west

;; a function with (meta-)data which are functions
(defn bot [x y bearing-num]
  {:coords     [x y]
   :bearing    ([:north :east :south :west] bearing-num)
   :forward    (fn [] (bot (+ x (:x (bearings bearing-num)))
                           (+ y (:y (bearings bearing-num)))
                           bearing-num))
   :turn-right (fn [] (bot x y (mod (+ 1 bearing-num) 4)))
   :turn-left  (fn [] (bot x y (mod (- 1 bearing-num) 4)))
   })

(:coords ( (:forward ( (:turn-right (bot 5 5 0))))))

;; thinking recursively

; mundane recursion -> stack overflow
(defn pow-mundane [base exp]
  (if (zero? exp)
    1
    (* base (pow base (dec exp)))))

; tail recursion with TCO
(defn pow [base exp]
  (letfn [(kapow [base exp acc]
            (if (zero? exp)
              acc
              (recur base (dec exp) (* acc base))))]
    (kapow base exp 1)))

; mutual recursion with trampolining
(defn elevator [commands]
  (letfn
      [(ff-open [[cmd & r]]
         #(case cmd
            :close (ff-closed r)
            :done  true
            false))
       (ff-closed [[cmd & r]]
         #(case cmd
            :open (ff-open r)
            :up   (sf-closed r)
            false))
       (sf-closed [[cmd & r]]
         #(case cmd
            :down (ff-closed r)
            :open (sf-open r)
            false))
       (sf-open [[cmd & r]]
         #(case cmd
            :close (sf-closed r)
            :done  true
            false))]
    (trampoline ff-open commands)))

; Continuation passing style
(defn fac-cps [n k]
  (letfn [(cont [v] (do  (println "cont" n v k)
                         (k (* n v))))]
    (do
      (println "fac-cps" n k)
      (if (zero? n)
        (k 1)
        (recur (dec n) cont)))))

(defn fac [n]
  (fac-cps n identity))

nil
