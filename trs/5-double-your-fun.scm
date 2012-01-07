;; The Reasoned Schemer
;; Chapter 5: Double your Fun

(load "mk.scm")
(load "mkextraforms.scm")
(load "mkprelude.scm")

; the goals which succeed and fail
(define U fail)
(define S succeed)

; = = = = = = = = = = = = = = = = = = = = = = = = = =

; Here is append
; it appends the list l to s, where s does 
; not have to be a list.
(define append
  (lambda (l s)
    (cond
     ((null? l) s)
     (else
      (cons (car l)
	    (append (cdr l) s))))))

(append '(a b c) '(d e f))
; > (a b c d e f)

(append '(a b c) 'd)
; > (a b c . d)

; The definition of appendo
(define appendo
  (lambda (l s out)
    (conde
     ((nullo l) (== s out))
     (else
      (fresh (a d res)
	     (conso a d l)
	     (conso a res out)
	     (appendo d s res))))))
; The order of the last two goals is important!
; TODO: Why?


;; some examples

(run* (x)
      (appendo '(cake) '(tastes yummy) x))
; > ((cake tastes yummy))

(run 5 (x)
     (fresh (y)
	    (appendo `(cake with ice . ,y) '(d t) x)))

(run* (x)
      (fresh (z)
	    (appendo '(cake with ice cream) `(d t . ,z) x)))
; > ((cake with ice cream d t . _.0))

; TODO
(run* (r)
     (fresh (x y)
	    (appendo x y '(cake with ice d t))
	    (== `(,x ,y) r)))
; > ((() (cake with ice d t))
;    ((cake) (with ice d t))
;    ((cake with) (ice d t))
;    ((cake with ice) (d t))
;    ((cake with ice d) (t))
;    ((cake with ice d t) ())) 

(run 7 (r)
     (fresh (x y z)
	    (appendo x y z)
	    (== `(,x ,y ,z) r)))

; This is like appendo, with the both conde lines swapped.
(define swappendo
  (lambda (l s out)
    (conde
     (S (fresh (a d res)
	       (conso a d l)
	       (conso a res out)
	       (swappendo d s res)))
     (else (nullo l) (== s out)))))

; This has no value, because x, y and z remain fresh for
; the recursive call.
; (run 1 (z)
;      (fresh (x y)
;             (swappendo x y z)))

; redefine swappendo with limited recursion depth.
(define swappendo
  (lambda-limited 5 (l s out)
		  (conde
		   (S (fresh (a d res)
			     (conso a d l)
			     (conso a res out)
			     (swappendo d s res)))
		   (else (nullo l) (== s out)))))
(run 1 (z)
     (fresh (x y)
	    (swappendo x y z)))
; > ((_.0 _.1 _.2 _.3 . _.4))

(define unwrap
  (lambda (x)
    (cond
     ((pair? x) (unwrap (car x)))
     (else x))))

(unwrap '(((pizza))))
; > pizza

(unwrap '(((pizza pie) with) extra cheese))
; > pizza

(define unwrapo
  (lambda (x out)
    (conde
     ((pairo x) 
      (fresh (a)
	     (caro x a)
	     (unwrapo a out)))
     (else (== x out)))))

(run* (x)
      (unwrapo '(((pizza))) x))
; > (pizza (pizza) ((pizza)) (((pizza))))

; This question does not have an answer.
; (run 1 (x)
;      (unwrapo x 'pizza))
; 

; To fix this: Swap the two conde lines.
(define unwrapo
  (lambda (x out)
    (conde
     (S (== x out))
     (else
      (fresh (a)
	     (caro x a)
	     (unwrapo a out))))))
; If you dont understand, reconsider the law of conde.

; Now these questions all have a value.
(run 5 (x)
     (unwrapo x 'pizza))

(run 5 (x)
     (unwrapo x '((pizza))))

(run 5 (x)
     (unwrapo `((,x)) 'pizza))

; flatten flattens e.g. lists of lists to lists
(define flatten
  (lambda (s)
    (cond
     ((null? s) '())
     ((pair? s)
      (append
       (flatten (car s))
       (flatten (cdr s))))
     (else (cons s '())))))

(define flatteno
  (lambda (s out)
    (conde
     ((nullo s) (== '() out))
     ((pairo s)
      (fresh (a d res-a res-d)
	     (conso a d s)
	     (flatteno a res-a)
	     (flatteno d res-d)
	     (appendo res-a res-d out)))
     (else (conso s '() out)))))

;; some examples

(run 1 (x)
     (flatteno '((a b) c (d e)) x))
; > ((a b c d e))

; this is a surprise!
(run* (x)
      (flatteno '(a) x))
; >  ((a)
;     (a ())
;     ((a)))

; All this lists would flatten to (a)

(run* (x)
      (flatteno '((a)) x))
; > ((a) (a ()) (a ()) (a () ()) ((a)) ((a) ()) (((a))))

(run* (x)
      (flatteno '(((a))) x))
; > ((a)
;    (a ())
;    (a ())
;    (a () ())
;    (a ())
;    (a () ())
;    (a () ())
;    (a () () ())
;    ((a))
;    ((a) ())
;    ((a) ())
;    ((a) () ())
;    (((a)))
;    (((a)) ())
;    ((((a)))))

(run* (x)
      (flatteno '((a b) c) x))
; > ((a b c)
;    (a b c ())
;    (a b (c))
;    (a b () c)
;    (a b () c ())
;    (a b () (c))
;    (a (b) c)
;    (a (b) c ())
;    (a (b) (c))
;    ((a b) c)
;    ((a b) c ())
;    ((a b) (c))
;    (((a b) c)))

; The following question does not succeed
;(run 1 (x)
;     (flatteno x '((a b) c)))

(define flattenrevo
  (lambda (s out)
    (conde
     (S (conso s '() out))
     ((nullo s) (== '() out))
     (else
      (fresh (a d res-a res-d)
	     (conso a d s)
	     (flattenrevo a res-a)
	     (flattenrevo d res-d)
	     (appendo res-a res-d out))))))

; Now the following question succeeds,
; but the third value would never been found.
(run 2 (x)
     (flattenrevo x '((a b) c)))
; > (((a b) . c) 
;    ((a b) c))


(reverse
 (run* (x)
       (flattenrevo '((a b) c) x)))

(length
 (run* (x)
       (flattenrevo '((((a (((b))) c))) d) x)))
; > 574

;; About swapping conde lines
;; TODO

;;;
'()
