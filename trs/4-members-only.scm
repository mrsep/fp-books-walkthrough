;; The Reasoned Schemer
;; Chapter 4: Members Only

(load "mk.scm")
(load "mkextraforms.scm")
(load "mkprelude.scm")

; the goals which succeed and fail
(define U fail)
(define S succeed)

; = = = = = = = = = = = = = = = = = = = = = = = = = =

(define eq-car?
  (lambda (l x)
    (eq? (car l) x)))

;; This function is not a predicate, 
;; like null?, list?, lol?, member?.
(define mem
  (lambda (x l)
    (cond
     ((null? l) #f)
     ((eq-car? l x) l)
     (else (mem x (cdr l))))))

;; How differs the definition of memo from
;; the definitions of listo, nullo, ...?
(define memo
  (lambda (x l out)
    (conde
     ((nullo l) U) ; this line is not necessary
     ((eq-caro l x) (== l out))
     (else
      (fresh (d)
	     (cdro l d)
	     (memo x d out))))))

(run* (out)
      (memo 'tofu '(a b d tofu e f g) out))
; > ((tofu e f g))

;; = = = =  The Second Commandment  = = = = =
;; To transform a function whose value is not
;; a Boolean into a function whose value is a
;; goal, add an extra argument to hold its
;; value, replace cond with conde, and unnest
;; each question and answer.
;; = = = = = = = = = = = = = = = = = = = = =

;; some nice examples

(run 1 (out)
     (fresh (x)
	    (memo 'tofu `(a b ,x d tofu e) out)))
; > ((tofu d tofu e))

(run* (r)
      (memo r
	    '(a b tofu d tofu e)
	    '(tofu d tofu e)))
; > (tofu)

(run* (x)
      (memo 'tofu '(tofu e) `(,x e)))
; > (tofu)

(run* (out)
      (fresh (x)
	     (memo 'tofu `(a b ,x d tofu e) out)))
; > ((tofu d tofu e) (tofu e))

; Solutions z are lists for which the goal (memo 'tofu ...z) 
; succeeds. It succeeds for arbitrary lists z two times, 
; since 'tofu is two times element of the list. Every 
; other solution must have at least on 'tofu as a member.
(run 12 (z)
     (fresh (u)
	    (memo 'tofu `(a b tofu d tofu e . ,z) u)))

; removes one occurence of x in the list l.
(define rember
  (lambda (x l)
    (cond
     ((null? l) '())
     ((eq-car? l x) (cdr l))
     (else 
      (cons (car l)
	    (rember x (cdr l)))))))

(define rembero
  (lambda (x l out)
    (conde
     ((nullo l) (== '() out))
     ((eq-caro l x) (cdro l out))
     (else
      (fresh (a d res)
	     (conso a d l)
	     (rembero x d res)
	     (conso a res out))))))

(run* (out)
      (fresh (y)
	     (rembero 'peas `(a b ,y d peas e) out)))
; > ((a b d peas e) 
;    (a b _.0 d e))

; 1. sol: y gets a, the first a is removed
; 2. sol: y gets b, the first b is removed
; 3. sol: y has been removed, whatever its values is
; 4. sol: y gets d, the first d is removed
; 5. sol: z has been removed
; 6 .sol: y gets e, the first e is removed
; 7. sol: nothing has been removed
(run* (out)
      (fresh (y z)
	     (rembero y `(a b ,y d ,z e) out)))
; > ((b a d _.0 e)
;   (a b d _.0 e)
;   (a b d _.0 e)
;   (a b d _.0 e)
;   (a b _.0 d e)
;   (a b e d _.0)
;   (a b _.0 d _.1 e))

; 1. sol: ... TODO
(run* (r)
      (fresh (y z)
	     (rembero y `(,y d ,z e) `(,y d e))
	     (== `(,y ,z) r)))
; > ((d d) 
;    (d d) 
;    (_.0 _.0) 
;    (e e))

; sol 1: 
(run 13 (w)
     (fresh (y z  out)
	    (rembero y `(a b ,y d ,z . ,w) out)))
; > (_.0
;    _.0
;    _.0
;    _.0
;    _.0
;    ()
;    (_.0 . _.1)
;    (_.0)
;    (_.0 _.1 . _.2)
;    (_.0 _.1)
;    (_.0 _.1 _.2 . _.3)
;    (_.0 _.1 _.2)
;    (_.0 _.1 _.2 _.3 . _.4))


;; ATTENTION
;; surpriseo should succeed for all values, other
;; than a b and c.
(define surpriseo
  (lambda (s)
    (rembero s '(a b c) '(a b c))))

; surpriseo leaves r fresh.
(run* (r)
      (surpriseo r))
; > (_.0)

; Why does surpriseo succeed here?
(run* (r)
      (surpriseo 'a))
; > (_.0)

; This makes no sense!
(run* (r)
      (surpriseo r)
      (== 'b r))
(run* (r)
      (== 'b r)
      (surpriseo r))
; > (b)




;;;
'()
