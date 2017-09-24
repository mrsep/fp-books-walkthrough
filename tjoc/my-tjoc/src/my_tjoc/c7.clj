(ns my-tjoc.c7)

;;; chapter 7 - Functional Programming

;; functions are first class citizens - they can be handled like any other data type
;; create, store, pass and return them

; collections can act as functions
(map [:chthon :phthor :beowulf :grendel] '(1 3))

; function application with apply
(comment
  (apply + '(5 6))
  )

; evaluation of S-exp
(comment
  ; data is code
  (eval '(+ 5 6))
  )

; composition of functions with comp
(def add4 (comp inc inc inc inc))

(def num-id (apply comp (list inc dec inc dec)))

(defn fnth [f n]
  (apply comp
         ; handle functions like data
         (take n (repeat f))))

(comment
  (add4 1)
  (num-id 42)
  ; create a function an immediately call it
  ((fnth inc 4) 0)
  )

; partial functions (which is not currying)
(def incsum (partial + 1))
(def prefix-a (partial cons "a"))

; also: functions can act as data, and they are data (stored in a map)
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

; Higher order functions (prefer them when processing sequences)
; map filter reduce sort-by for some repeatedly keep take-while drop-while

; write pure functions to achieve referential transparency

; pre and post conditions
(defn slope [p1 p2]
  {:pre [(not= p1 p2) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  (/ (- (p2 1) (p1 1))
     (- (p2 0) (p1 0))))

; decoupled constraints for the function f
(defn constraint [f m]
  {:post [(= (:meat %) (:meat m))]}
  (f m))

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
    (* base (pow-mundane base (dec exp)))))

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
; no tail call optimization
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

(defn mk-cps [accept? end-value kend kont]
  (fn [n]
    ((fn [n k]
       (let [cont (fn [v] (k (kont v n)))]
         (if (accept? n)
           (k end-value)
           (recur (dec n) cont))))
     n kend)))

; call hierarchy of mk-cps
;cont1 = kend . f
;cont2 = cont1 . f = kend . f . f
;...
;contn = contn-1 . f = kend . f^n

(def fac (mk-cps zero? 1 identity #(* %1 %2)))
(def tri (mk-cps zero? 1 dec #(+ %1 %2)))

;; A* path-finding

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
           (map (comp vec #(map + yx %))
                deltas))))

(def world [[  1   1   1   1   1]
            [999 999 999 999   1]
            [  1   1   1   1   1]
            [  1 999 999 999 999]
            [  1   1   1   1   1]])

(defn estimate-cost [step-cost-est size y x]
  (* step-cost-est
     (- (+ size size) y x 2)))

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
     (:cost cheapest-nbr 0)))

(defn total-cost [newcost step-cost-est size y x]
  (+ newcost
     (estimate-cost step-cost-est size y x)))

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min this]
              (if (> (f min) (f this)) this min))
            coll)))

(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (replicate size (vec (replicate size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo)
        [(peek (peek routes)) :steps steps] ; terminate with best route
        (let [[_ yx :as work-item] (first work-todo)
              rest-work-todo (disj work-todo work-item)
              nbr-yxs (neighbors size yx)
              cheapest-nbr (min-by :cost
                                   (keep #(get-in routes %)
                                         nbr-yxs))
              newcost (path-cost (get-in cell-costs yx) cheapest-nbr)
              oldcost (:cost (get-in routes yx))]
          (if (and oldcost (>= newcost oldcost))
            (recur (inc steps) routes rest-work-todo)
            (recur (inc steps)
                   (assoc-in routes yx
                             {:cost newcost
                              :yxs (conj (:yxs cheapest-nbr [])
                                         yx)})
                   (into rest-work-todo
                         (map
                          (fn [w]
                            (let [[y x] w]
                              [(total-cost newcost step-est size y x) w]))
                          nbr-yxs)))))))))

(defn random-world [size maxn]
  (vec (repeatedly size
                   #(vec (repeatedly size
                                     (partial rand-int maxn))))))

nil
