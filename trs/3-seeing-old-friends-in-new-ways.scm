;; The Reasoned Schemer
;; Chapter 3: Seeing old Friends in new Ways

(load "mk.scm")
(load "mkextraforms.scm")
(load "mkprelude.scm")

; the goals which succeed and fail
(define U fail)
(define S succeed)

;; Compare the definition of list? with 
;; the one for listo.

(define list?
  (lambda (l)
    (cond
     ((null l) #t)
     ((pair? l) (list? (cdr l)))
     (else #f))))

(define listo
  (lambda (l)
    (conde
     ((nullo l) S)
     ((pairo l)
      (fresh (d)
	     (cdro l d)
	     (listo d)))
     (else U))))

;; How do they relate?

;; = = = = =  The First Commandment  = = = =
;; To transform a function whose value is a 
;; Boolean into a function whose value is a
;; goal, replace cond with conde and unnest
;; each question and answer. 
;; Unnest the answer #t (or #f) by replacing
;; it with S (or U).
;; = = = = = = = = = = = = = = = = = = = = =

;; The goal succeeds for all values of x, 
;; therefore x remains fresh.
(run* (x)
      (listo `(a b ,x d)))
; > (_.0)

(run 1 (x)
     (listo `(a b c . ,x)))
; > (())

(run 5 (x)
     (listo `(a b c . ,x)))
; > ...

;; run* would produce an unbounded number of values
;; due to recursion. The goals from the second chapter
;; are not recursive.

;; List of Lists?
(define lol?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((list? (car l) (lol? (cdr l))))
     (else #f))))

(define lolo
  (lambda (l)
    (conde
     ((nullo l) S)
     ((fresh (a)
	     (caro l a)
	     (listo a))
      (fresh (d)
	     (cdro l d)
	     (lolo d)))
     (else U))))

; some list of lists
(run 10 (l)
      (lolo l))

(run* (q)
      (fresh (x y)
	     (lolo `((a b) (,x c) (d ,y)))
	     (== #t q)))
; > (#t)

(run 1 (q)
     (fresh (x)
	    (lolo `((a b) . ,x))
	    (== #t q)))
; > (#t)
; since
(run 1 (x)
     (lolo `((a b) . ,x)))
; > (())


;; Twins and List of Twins

(define twinso
  (lambda (s)
    (fresh (x y)
	   (conso x y s)
	   (conso x '() y))))

(define twinso
  (lambda (s)
    (fresh (x)
	   (== `(,x ,x) s))))

;; List of twins
(define loto
  (lambda (l)
    (conde
     ((nullo l) S)
     ((fresh (a)
	     (caro l a)
	     (twinso a))
      (fresh (d)
	     (cdro l d)
	     (loto d)))
     (else U))))

;; some examples
(run* (t)
     (twinso t))

(run* (z)
      (twinso `(,z tofu)))

(run 5 (z)
     (loto z))

(run 5 (z)
     (loto `((1 1) (2 2) (3 3) ,z)))

(run 5 (z)
     (fresh (x y)
	    (loto `((,x ,y)))
	    (== z `(,x ,y))))

(run 5 (z)
     (loto `((1 ,z) (2 ,z))))

(run 5 (r)
     (fresh (w x y z)
	    (loto `((g g) (e ,w) (,x ,y) . ,z))
	    (== `(,w (,x ,y) ,z) r)))

;; List of whatever
(define listofo
  (lambda (predo l)
    (conde
     ((nullo l) S)
     ((fresh (a)
	     (caro l a)
	     (predo a))
      (fresh (d)
	     (cdro l d)
	     (listofo predo d)))
     (else U))))

(define loto
  (lambda (l)
    (listofo twinso l)))

;; The old friend member?

(define member?
  (lambda (x l)
    (cond 
     ((null? l) #f)
     ((eq-car? l x) #t)
     (else (member? x (cdr l))))))

(define eq-car?
  (lambda (l x)
    (eq? (car l) x)))

(define eq-caro
  (lambda (l x)
    (caro l x)))

(define membero
  (lambda (x l)
    (conde
     ((nullo l) U) ; this line is unnecessary!
     ((eq-caro l x) S)
     (else
      (fresh (d)
	     (cdro l d)
	     (membero x d))))))

(run 5 (x)
     (membero x '(1 2 3)))
; > (1 2 3)

(define identity
  (lambda (l)
    (run* (y)
	  (membero y l))))

(run* (x)
      (membero 3 `(1 2 ,x 4 5)))
; > (3)

(run 1 (x)
     (membero 'e `(pasta e ,x fragioli)))
; > (_.0)

(run* (r)
      (fresh (x y)
	     (membero 'e `(pasta ,x fragioli ,y))
	     (== `(,x ,y) r)))
; > ((e _.0) (_.0 e))


(run 5 (l)
     (membero 42 l))
; > ((42 . _.0)
;    (_.0 42 . _.1)
;    (_.0 _.1 42 . _.2)
;    (_.0 _.1 _.2 42 . _.3)
;    (_.0 _.1 _.2 _.3 42 . _.4))


(define pmembero
  (lambda (x l)
    (conde
     ((nullo l) U) ; this line is unnecessary!
     ((eq-caro l x) (cdro l '()))
     (else
      (fresh (d)
	     (cdro l d)
	     (pmembero x d))))))

(run 5 (l)
     (pmembero 42 l))
; > ((42) 
;    (_.0 42) 
;    (_.0 _.1 42) 
;    (_.0 _.1 _.2 42) 
;    (_.0 _.1 _.2 _.3 42))

(run* (q)
      (pmembero 'tofu '(a b tofu d tofu))
      (== #t q))
; > (#t)

(run* (q)
      (pmembero 42 '(1 42 3))
      (== #t q))
; > (), what???

; redefine pmembero
(define pmembero
  (lambda (x l)
    (conde
     ((nullo l) U) ; this line is unnecessary!
     ((eq-caro l x) (cdro l '()))
     ((eq-caro l x) S)
     (else
      (fresh (d)
	     (cdro l d)
	     (pmembero x d))))))

(run* (q)
      (pmembero 42 '(1 42 3))
      (== #t q))
; > (#t)

(run* (q)
      (pmembero 42 '(1 42 3 42 5 42))
      (== #t q))
; > (#t #t #t #t)

; a more refined definition of pmembero
(define pmembero
  (lambda (x l)
    (conde
     ((nullo l) U) ; this line is unnecessary!
     ((eq-caro l x) (cdro l '()))
     ((eq-caro l x)
      (fresh (a d)
	     (cdro l `(,a . ,d))))
     (else
      (fresh (d)
	     (cdro l d)
	     (pmembero x d))))))

(run* (q)
      (pmembero 42 '(1 42 3 42 5 42))
      (== #t q))
; > (#t #t #t)

; the odd lines are from the second conde line
; the even lines are fomr the third conde line
(run 12 (l)
     (pmembero 'tofu l))
; > ((tofu)
;    (tofu _.0 . _.1)
;    (_.0 tofu)
;    (_.0 tofu _.1 . _.2)
;    (_.0 _.1 tofu)
;    (_.0 _.1 tofu _.2 . _.3)
;    (_.0 _.1 _.2 tofu)
;    (_.0 _.1 _.2 tofu _.3 . _.4)
;    (_.0 _.1 _.2 _.3 tofu)
;    (_.0 _.1 _.2 _.3 tofu _.4 . _.5)
;    (_.0 _.1 _.2 _.3 _.4 tofu)
;    (_.0 _.1 _.2 _.3 _.4 tofu _.5 . _.6))

(define first-value
  (lambda (l)
    (run 1 (y)
	 (membero y l))))

(first-value '(pasta e fragioli))
; > (pasta)

(define memberrevo
  (lambda (x l)
    (conde
     ((nullo l) U)
     (S
      (fresh (d)
	     (cdro l d)
	     (memberrevo x d)))
     (else (eq-caro l x)))))

(run* (x)
      (memberrevo x '(pasta e fragioli)))

(define reverse-list
  (lambda (l)
    (run* (y)
	  (memberrevo y l))))

;;;
'()
