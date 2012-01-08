;; The Reasoned Schemer
;; Chapter 7: A Bit too much

(load "mk.scm")
(load "mkextraforms.scm")
(load "mkprelude.scm")

; the goals which succeed and fail
(define U fail)
(define S succeed)

; = = = = = = = = = = = = = = = = = = = = = = = = = =

; The values 0 and 1 are bits.
; Binary variables hold the values 0 or 1.

; some binary operators
; there are alternative definitions, utilizing bit-nando

(define bit-noto
  (lambda (x r)
    (conde
     ((== 0 x) (== 1 r))
     ((== 1 x) (== 0 r))
     )))

(define bit-oro
  (lambda (x y r)
    (conde
     ((== 0 x) (== 0 y) (== 0 r))
     ((== 1 x) (== 0 y) (== 1 r))
     ((== 0 x) (== 1 y) (== 1 r))
     ((== 1 x) (== 1 y) (== 1 r))
     )))

(define bit-ando
  (lambda (x y r)
    (conde
     ((== 0 x) (== 0 y) (== 0 r))
     ((== 1 x) (== 0 y) (== 0 r))
     ((== 0 x) (== 1 y) (== 0 r))
     ((== 1 x) (== 1 y) (== 1 r))
     )))

(define bit-xoro
  (lambda (x y r)
    (conde
     ((== 0 x) (== 0 y) (== 0 r))
     ((== 1 x) (== 0 y) (== 1 r))
     ((== 0 x) (== 1 y) (== 1 r))
     ((== 1 x) (== 1 y) (== 0 r))
     )))

(define bit-nando
  (lambda (x y r)
    (conde
     ((== 0 x) (== 0 y) (== 1 r))
     ((== 1 x) (== 0 y) (== 1 r))
     ((== 0 x) (== 1 y) (== 1 r))
     ((== 1 x) (== 1 y) (== 0 r))
     )))

(run* (s)
     (fresh (x y)
	    (bit-xoro x y 1)
	    (== `(,x ,y) s)))
; > ((1 0) (0 1))

(define half-addero
  (lambda (x y r c)
    (all
     (bit-xoro x y r)
     (bit-ando x y c))))

(define full-addero
  (lambda (b x y r c)
    (fresh (w xy wz)
	   (half-addero x y w xy)
	   (half-addero w b r wz)
	   (bit-xoro xy wz c))))

(run* (s)
      (fresh (r c)
	     (full-addero 0 1 1 r c)
	     (== `(,r ,c) s)))

; Numbers are integers greater than or equal to zero.
; They can be represented by a list of bits.
; The number zero is represented by the list ().
; The least significand bit is the car of the list.

(define twiceo
  (lambda (n res)
    (== res `(0 . ,n))))

(run* (s)
      (twiceo s '(0 1 0 1)))
; > (1 0 1)

; Convert integers in number representations.
(define build-num
  (lambda (n)
    (cond
     ((odd? n) 
      (cons 1 
	    (build-num (/ (- n 1) 2))))
     ((and (not (zero? n)) 
	   (even? n)) 
      (cons 0 
	    (build-num (/ n 2))))
     ((zero? n) '()))))

; build-num satisfies the non-overlapping property, 
; the cond-lines can be rearranged in any order.

(build-num 42)
; > (0 1 0 1 0 1)

; some predicates over numbers as relations

(define zeroo
  (lambda (n)
    (== '() n)))

(define poso
  (lambda (n)
    (fresh (a d)
	   (== `(,a . ,d) n))))

(define oddo
  (lambda (n)
    (fresh (d)
	   (== `(1 . ,d) n))))

(define eveno
  (lambda (n)
    (conde
     ((zeroo n) S)
     (else
      (fresh (d)
	     (== `(0 . ,d) n))))))

(define >1o
  (lambda (n)
    (fresh (a ad dd)
	   (== `(,a ,ad . ,dd) n))))

; playground
(run* (q)
      (eveno q)
      (zeroo q))

; What about prime numbers?
; We wait until there something like 
; multiplication or division.

(run 3 (s)
     (fresh (x y r)
	    (addero 0 x y r)
	    (== `(,x ,y ,r) s)))
; > ((_.0 () _.0) 
;    (() (_.0 . _.1) (_.0 . _.1)) 
;    ((1) (1) (0 1)))

; Here the non-overlapping comes into play, since the ground
; and the nonground values are not related.

; Ground values are values without variables, the
; other are called nonground values.

;; Arithemtic

(define addero
  (lambda (d n m r)
    (condi
      ((== 0 d) (== '() m) (== n r))
      ((== 0 d) (== '() n) (== m r)
       (poso m))
      ((== 1 d) (== '() m)
       (addero 0 n '(1) r))
      ((== 1 d) (== '() n) (poso m)
       (addero 0 '(1) m r))
      ((== '(1) n) (== '(1) m)
       (fresh (a c)
         (== `(,a ,c) r)
         (full-addero d 1 1 a c)))
      ((== '(1) n) (gen-addero d n m r))
      ((== '(1) m) (>1o n) (>1o r)
       (addero d '(1) n r))
      ((>1o n) (gen-addero d n m r))
      (else fail))))

(define gen-addero
  (lambda (d n m r)
    (fresh (a b c e x y z)
      (== `(,a . ,x) n)
      (== `(,b . ,y) m) (poso y)
      (== `(,c . ,z) r) (poso z)
      (alli
        (full-addero d a b c e)
        (addero e x y z)))))

; the numbers which sum to five
(run* (s)
      (fresh (x y)
	     (addero 0 x y '(1 0 1))
	     (== `(,x ,y) s)))
; > (((1 0 1) ())
;    (() (1 0 1))
;    ((1) (0 0 1))
;    ((0 0 1) (1))
;    ((1 1) (0 1))
;    ((0 1) (1 1)))

(define +o
  (lambda (n m k)
    (addero 0 n m k)))

; again the numbers which sum up to five
(run* (s)
      (fresh (x y)
	     (+o x y (build-num 5))
	     (== `(,x ,y) s)))
; > (((1 0 1) ())
;    (() (1 0 1))
;    ((1) (0 0 1))
;    ((0 0 1) (1))
;    ((1 1) (0 1))
;    ((0 1) (1 1)))

(define -o
  (lambda (n m k)
    (addero 0 m k n)))

(run* (q)
      (-o '(0 0 0 1) '(1 0 1) q))
; > ((1 1))

(run* (q)
      (-o (build-num 6) (build-num 8) q))
; > ()


;;;
'()
