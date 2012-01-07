;; The Reasoned Schemer
;; Chapter 2: Teaching old Toys new Tricks

(load "mk.scm")
(load "mkextraforms.scm")
(load "mkprelude.scm")

; the goals which succeed and fail
(define U fail)
(define S succeed)

; = = = = = = = = = = = = = = = = = = = = = = = = = =

; r gets associated with the car of (a c o r n).
(run* (r)
      (caro '(a c o r n) r))
; > (a)

; q gets associated with #t in the second goal, 
; and the first goal succeeds.
(run* (q)
      (caro '(a c o r n) 'a)
      (== #t q))
; > (#t)

; x gots associated with r, since r is the car of
; (r x). Then x gots associated with pear and 
; therefore r is associated with pear.
(run* (r)
      (fresh (x y)
	     (caro `(,r ,y) x)
	     (== 'pear x)))
; > (pear)

; The definition of caro
(define caro
  (lambda (p a)
    (fresh (d)
	   (== (cons a d) p))))

; Solutions are pairs of x and y, where x and y are
; each cars of a list.
(run* (r)
      (fresh (x y)
	     (caro '(grape raisin pear) x)
	     (caro '((a) (b) (c)) y)
	     (== (cons x y) r)))
; > ((grape a))

; v gots associated with the cdr of (a c o r n) and
; r with the car of v.
(run* (r)
      (fresh (v)
	     (cdro '(a c o r n) v)
	     (caro v r)))
; > (c)

; The definition of cdro
(define cdro
  (lambda (p d)
    (fresh (a)
	   (== (cons a d) p))))

;; No more comments necessary.

(run* (r)
      (fresh (x y)
	     (cdro '(grape raisin pear) x)
	     (caro '((a) (b) (c)) y)
	     (== `(,x ,y) r)))
; > ((raisin pear) a)

(run* (q)
      (cdro '(a c o r n) '(c o r n))
      (== #t q))
; > (#t)

(run* (x)
      (cdro '(c o r n) `(,x r n)))
; > (o)

(run* (l)
      (fresh (x)
	     (cdro l '(c o r n))
	     (caro l x)
	     (== 'a x)))
; > ((a c o r n))

; l gots associated with the (cons (a b c) (d e)).
(run* (l)
      (conso '(a b c) '(d e) l))
; > (((a b c) d e))

; x gots associated hence the cons of x and (a b c)
; is (d a b c).
(run* (x)
      (conso x '(a b c) '(d a b c)))
; > (d)

; The first goal associates r with a 4-tuple, whose 
; last element is the fresh variable x. The second 
; goal associates c to the last element of r.
(run* (r)
      (fresh (x y z)
	     (== `(e a d ,x) r)
	     (conso y `(a ,z c) r)))
; > ((e a d c))

;; No more comments necessary.
(run* (x)
      (conso x `(a ,x c) `(d a ,x c)))
; > (d)

(run* (l)
      (fresh (x)
	     (== `(d a ,x c) l)
	     (conso x `(a ,x c) l)))
(run* (l)
      (fresh (x)
	     (conso x `(a ,x c) l)
	     (== `(d a ,x c) l)))
; > ((d a d c))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(run* (l)
      (fresh (d x y w s)
	     (conso w '(a n s) s)
	     (cdro l s)
	     (caro l x)
	     (== 'b x)
	     (cdro l d)
	     (caro d y)
	     (== 'e y)))
; > ((b e a n s))

; Definition of nullo and eqo, both are
; parametrized goals.
(define nullo
  (lambda (x)
    (== '() x)))

(define eqo
  (lambda (x y)
    (== x y)))

(run* (x)
      (nullo x))
; > (())

;; Some stuff about pairs

(and
 (pair? '(split . pea))
 (pair? '(split))
 (not (pair? 'split)))
; > #t


(run* (r)
      (fresh (x y)
	     (== (cons x (cons y 'salad)) r)))
; > ((_.0 _.1 . salad))

(define pairo
  (lambda (p)
    (fresh (a d)
	   (conso a d p))))

; Some more simple examples

(run* (q)
      (pairo (cons q q))
      (== #t q))
; > (#t)

(run* (q)
      (pairo '())
      (== #t q))
; > ()

(run* (x)
      (pairo x))
; > ((_.0 . _.1))

(run* (r)
      (pairo (cons r 'pear)))
; > (_.0)


;;;
'()
