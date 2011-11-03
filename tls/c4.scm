;; The Little Schemer
;; Chapter 4 - Numbers Games

;; do you re(me)mber our last session?
(load "c1.scm")

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define op+
  (lambda (a b)
    (cond
     ((zero? b) a)
;     (else (op+ (add1 a) (sub1 b)))))) ; my idea => tail recursion
     (else (add1 (op+ a (sub1 b)))))))  ; eles idea => my poor stack

(define op-
  (lambda (a b)
    (cond
     ((zero? b) a)
     (else (sub1 (op- a (sub1 b)))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (op+ (car tup) (addtup (cdr tup)))))))

(define op*
  (lambda (a b)
    (cond
     ((zero? b) 0)
     (else (op+ a (op* a (sub1 b)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (op+ (car tup1) (car tup2))
		 (tup+ (cdr tup1) (cdr tup2)))))))

(define rel<
  (lambda (a b)
    (cond
     ((zero? b) false)
     ((zero? a) true)
     (else (rel< (sub1 a) (sub1 b))))))

(define rel>
  (lambda (a b)
    (cond
     ((zero? a) false)
     ((zero? b) true)
     (else (rel> (sub1 a) (sub1 b))))))

(define rel=
  (lambda (a b)
    (cond
     ((zero? a) (zero? b))
     ((zero? b) false)
     (else (rel= (sub1 a) (sub1 b))))))

(define pow
  (lambda (b e)
    (cond
     ((zero? e) 1)
     (else (op* b (pow b (sub1 e)))))))

(define op/
  (lambda (a b)
    (cond
     ((op< a b) 0)
     (else (add1 (op/ (op- a b) b))))))

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat) 
		 (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

;; equality of atoms and numbers
(define eqan?
  (lambda (a b)
    (cond
     ((and (atom? a) (atom? b)) (eq? a b))
     ((and (number? a) (number? b)) (= a b))
     (else false))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? a (car lat)) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (rel= 1 n)))
