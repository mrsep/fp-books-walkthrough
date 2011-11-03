;; The Little Schemer
;; Chapter 9 - ... and Again, and Again, and Again, ...

(load "c8.scm")


(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

'(6 2 4 caviar 5 7 3)

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn) 
      (keep-looking a (pick sorn lat) lat))
     (else (eq? a sorn)))))

;; looking is called a partial function, because there are arguments
;; for which it does not give an answer

;; the most partial function
(define eternity
  (lambda (x)
    (eternity x)))

; ((a b) c) -> (a (b c))
(define shift
  (lambda (p)
     (build (first (first p)) 
	    (build (second (first p)) (second p)))))

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else (build (first pora)
		  (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (+ (length* (first pora))
	 (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (+ (* 2 (weight* (first pora)))
	 (weight* (second pora)))))))

; (weight*        '((a b) c))
; (weight* (align '((a b) c)))

(define shuffle
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (shuffle (revpair pora)))
     (else (build (first pora)
		  (shuffle (second pora)))))))

(define collatz
  (lambda (n)
    (cond
     ((one? n) 1)
     (else
      (cond
       ((even? n) (collatz (/ n 2)))
       (else (collatz (add1 (* 3 n)))))))))

(define ackermann
  (lambda (n m)
    (cond
     ((zero? n) (add1 m))
     ((zero? m) (ackermann (sub1 n) 1))
     (else (ackermann (sub1 n)
		      (ackermann n (sub1 m)))))))

;; What are recursive definitions?

; length_0
(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 (eternity (cdr l))))))

; length_1
(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 (length_0 (cdr l))))))

;; now without define(define length_1

; length_1
(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 
	  ((lambda (l)
	     (cond
	      ((null? l) 0)
	      (else (add1 (eternity (cdr l)))))) 
	   (cdr l))))))

; length_2
(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1
	  ((lambda (l)
	    (cond
	     ((null? l) 0)
	     (else (add1 
		    ((lambda (l)
		       (cond
			((null? l) 0)
			(else (add1 (eternity 
				     (cdr l))))))
		    (cdr l)))))))
	   (cdr l)))))

; What is recursion?

; 9. Commandment: Abstract common patterns with a new function!

; --> length_0
((lambda (f)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (f (cdr l)))))))
   eternity)

; length_1
((lambda (f)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (g (cdr l)))))))
  eternity))

; length_2
((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
	((null? l) 0)
	(else (add1 (length (cdr l)))))))
   eternity)))

; still patterns there 

; length_0

; a function that builds the former function length_0
; it is a function applies the function eternity to length_0

((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

; length_1

((lambda (mk-length)
    (mk-length
     (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

; length_2

((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

; length_3

((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length
      (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

; a finite tower, until eternity

; length_0

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
       (else (add1 (mk-length (cdr l))))))))

; length_1

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 
	     ((mk-length eternity)
	      (cdr l))))))))

; length

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 
	     ((mk-length mk-length)
	      (cdr l))))))))

; own descriptions
;; application of the argument to itself, therefor the argument is a function
;; which takes a function as argument
(define self
  (lambda (f) (f f)))

;; like-length is a example of such a function
;; it takes a argument, which is a function and maybe self-applicates it 
;; usually, the argument is like-length itself to realize recursion
(define like-length
  (lambda (fun)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1
	      ((self fun)
	       (cdr l))))))))

;; the self application of such a function makes length, now applicate a list to it
(define length_oo
  (lambda (l)
    ((self like-length) l)))

;; some more steps: recreate length & applicate the list to it

; ((lambda (mk-length)
;    (mk-length mk-length))
;  (lambda (mk-length)
;    ((lambda (length)
;       (lambda (l)
; 	(cond
; 	 ((null? l) 0)
; 	 (else (add1 (length (cdr l)))))))
;     (mk-length mk-length))))
;; but resuls in infinite self application/compilation, even if a list is applied

;; therefore: the application (mk-length mk-length) makes length
;;            a list has to be applied to it ((mk-length mk-length) l) and
;;            abstract from l with a lambda: 
;;            length == (lambda (l) ((mk-length mk-length) l)
;;            this is also called eta-expansion

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 
	     ((lambda (x) 
		(mk-length mk-length))
	      (cdr l))))))))

;; abstract the function away to get length back
   
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond
	 ((null? l) 0)
	 (else (add1 (length (cdr l)))))))
    (lambda (x) 
      ((mk-length mk-length) x)))))

;; now the two innermost lambdas dont depend on mk-length => move them out

((lambda (f)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (f
       (lambda (x) 
	 ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

;; the first part is called the applicative Y combinator, here called Y too
;; a eta-expansion of a part of the orginial Y, (the last (x x) part)
;; it is used in call-by-value (applicative order) evaluation
;;     Y == λf. (λx. x x) (λx. f (λy. x x y))
;; (Y g) -> (λx. x x) (λx. g (λy. x x y))    ; beta
;; some possibilities, but ?????
(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (x)
       (f (lambda (y) ((x x) y)))))))

; redefinition of length with this Y combinator
(define length_Y
  (lambda (l)
    ((Y (lambda (length)
	  (lambda (l)
	    (cond
	     ((null? l) 0)
	     (else (add1 (length (cdr l)))))))) l)))

;; here an other version from wikipedia
;;     Z == λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))
;; (Z g) -> (λx. g (λy. x x y)) (λx. g (λy. x x y))     ; alpha: y -> w
;;       -> (λx. g (λy. x x y)) (λx. g (λw. x x w))     ; beta
;;       -> g (λy. (λx. g (λw. x x w)) (λx. g (λw. x x w)) y)
;;       -> g (λy. (Z g) y)

;; and here the original Y combinator:
;;     Y == λf.(λx.f (x x)) (λx.f (x x))
;; (Y g) == λf.(λx.f (x x)) (λx.f (x x)) g   ; beta
;;       -> (λx.g (x x)) (λx.g (x x))        ; alpha
;;       -> (λy.g (y y)) (λx.g (x x))        ; beta
;;       -> g ( (λx.g (x x)) (λx.g (x x)) )
;;       == g (Y g)
(define Y-orig
  (lambda (f)
    ((lambda (x) (f (x x))) 
     (lambda (x) (f (x x))))))

;; length_Y-orig would result in infinite evaluation
;; because: 
;;   - scheme has call-by-value evaluation
;;   - remember: Y g --> g (Y g)
;;   - to evaluate this, the argument (Y g) has first to be evaluated
;;   - which results in g (Y g)
;;   - now we have g (g (Y g)) ad infinitum

;; on the other hand the applicative Y combinator Z
;;   - remember: (Z g) -> g (λy. (Z g) y)
;;   - to evaluate g, its argument (λy. (Z g) y) has to be evaluated
;;   - 