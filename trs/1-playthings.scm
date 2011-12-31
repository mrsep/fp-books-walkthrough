;; The Reasoned Schemer
;; Chapter 1: Playthings

(load "mk.scm")
(load "mkextraforms.scm")

; the goals which succeed and fail
(define U fail)
(define S succeed)

; There is no solution if all goals fail.
(run* (q) U)
; > ()

; q is not associated and therefore a fresh variable, 
; since all goals succeed without association to q.
; The symbol  _.0  represents a fresh variable.
(run* (q) S)
; > (_.0)

; The goal succeeds iff q unifies with #t.
; q is therefore associated with #t.
(run* (q)
      (== #t q))

; All goals have to succeed, but U fails.
(run* (q)
      U
      (== #t q))

; S always succeeds, therefore the solution 
; only depends on the last goal.
(run* (q)
      S
      (== #t q))

; The above mentioned is the same for all
; other values, e.g. #f, 1, "Hallo" and 'corn.
(run* (q)
      S
      (== 'corn q))

(run* (q)
      U
      (== 'corn q))

; The goal does not succeed, since #f is not equal to #t.
(run* (x)
 (let ((x #f))
   (== #t x)))

; A fresh variable is a variable without an association.
; (fresh (x...) goals...) introduces the new and fresh 
; variables and succeeds iff the goals succeed.

(run* (q)
      (fresh (x)
	     (== #t x)
	     (== x q)
	     (== #t q)))
; > (#t)

;; = = = = =  The Law of fresh  = = = = =
;; If x is fresh, then (== v x) succeeds
;; and associates x with v.
;; = = = = = = = = = = = = = = = = = = =

;; = = = = =  The Law of ==  = = = = = =
;; The order of the arguments does not 
;; matter, therefore == is a symmetric
;; relation:
;;   (== v w) is the same as (== w v)
;; = = = = = = = = = = = = = = = = = = =

; The variable in the innermost goal is the one
; introduced by fresh.
(run* (x)
      (let ((x #f))
	(fresh (x)
	       (== #t x))))
; > (_.0)

; For each fresh (different) variable, there is a
; symbol _.n, where n is a natural number. This
; entity is not a varaible, but shows, that the
; variable was fresh. 
;; Def: We say, that such a variable has been reified.
(run* (r)
      (fresh (x y)
	     (== (list x y) r)))
; names dont matter!
(run* (r)
      (fresh (u s)
	     (== (list u s) r)))
; > ((_.0 _.1))

; x and y are the same fresh variables.
(run* (r)
      (fresh (x)
	     (let ((y x))
	       (== (list y x y) r))))
; > ((_.0 _.0 _.0))

; the free variable x resulting from the inner fresh
; is different to the outer x and therefore different
; to the fresh variable y.
(run* (r)
      (fresh (x)
	     (let ((y x))
	       (fresh (x)
		      (== (list y x y) r)))))
; > ((_.0 _.1 _.0))

; q gets associated to #t for the first goal 
; to succeed and therefore is no longer fresh.
; Thus the second goal can not succeed.
(run* (q)
      (== #t q)
      (== #f q))
; > ()

; q is already associated to #f and therefore 
; the second goal succeeds too. 
(run* (q)
      (== #f q)
      (== #f q))
; > (#f)

; x and q are the same variable and got associated
; to #t in the goal.
(run* (q)
      (let ((x q))
	(== #t x)))
; > (#t)

; q and x are not associated and remain fresh. But stay
; connected to each other: If one variable would have
; been associated to a value, the other is associated 
; to the same value, They are associated to each other.
;; Def: Associated variables share or co-refer.
(run* (q)
      (fresh (x)
	     (== x q)))
; > (_.0)

; The order of the goals should not matter, Prove?
; q is fresh and then gets the association of x, 
; because they co-refer. q and x are different variables.
(run* (q)
      (fresh (x)
	     (== x q)
	     (== #t x)))
(run* (q)
      (fresh (x)
	     (== #t x)
	     (== x q)))
; > (#t)

; An old friend:
(cond
 (#f #t)
 (else #f))
; > #f

; This goal does not succeed.
(cond
 (#f S)
 (else U))
; > fail

; This goal does not succeed, too.
(conde
 (U S)
 (else U))

; This goal succeeds.
(conde
 (U U)
 (else S))

; q remains unassociated and the goal succeeds.
(run* (q)
      (conde
       (U U)
       (else S)))
; > (_.0)

;; Now it's getting interesting!
; The solution is the set of values for which all 
; goals succeed. The only goal is the conde from.
; It succeeds if x is fresh and gets asssociated
; with olive. If x is associated with oil, then 
; the first question (unify x with olive) fails
; and the second question is true. In both cases 
; the goal succeeds.
(run* (x)
      (conde
       ((== 'olive x) S)
       ((== 'oil x) S)
       (else U)))
; > (olive oil)

;; = = = = =  The Law of conde  = = = = =
;; To get more values from conde, pretend
;; that the successful conde line has 
;; failed, refreshing all variables that 
;; got an association from that line.
;; = = = = = = = = = = = = = = = = = = =

; The e in conde stands for every, since every
; line can succeed.

; (run 1 (x...) g...) produces at most one value.
(run 1 (x)
     (conde
      ((== 'olive x) S)
      ((== 'oil) S)
      (else U)))
; > (olive)

; The goal conde succeeds, if x is fresh or x is 
; associated with olive or oil.
(run* (x)
      (conde
       ((== 'virgin x) U)
       ((== 'olive x) S)
       (S S)
       ((== 'oil x) S)
       (else U)))
; > (olive _.0 oil)

; (run n (x...) g...) produces at most n values.
(run 2 (x)
     (conde
      ((== 'extra x) S)
      ((== 'virgin x) U)
      ((== 'olive x) S)
      ((== 'oil x) S)
      (else U)))
; > (extra olive)

; Solutions are pairs of x and y, where x is 
; associated with split and y with pea or
; x with navy and y with bean.
(run* (r)
      (fresh (x y)
	     (conde
	      ((== 'split x) (== 'pea y))
	      ((== 'navy x) (== 'bean y))
	      (else U))
	     (== (list x y) r)))
; > ((split pea) (navy bean))

; Definition of a parametrized goal.
(define teacupo
  (lambda (x)
    (conde
     ((== 'tea x) S)
     ((== 'cup x) S)
     (else U))))

(run* (x)
      (teacupo x))
; > (tea cup)

; Solutions are pairs of x and y, whith wether
; x is succeeds for the goal teacupo and y is
; associated with #t or x is associated with #f
; and y with #t. The two answers of the first 
; question in conde must both succeed hence the
; whole line can succeed.
(run* (r)
      (fresh (x y)
	     (conde
	      ((teacupo x) (== #t y) S)
	      ((== #f x) (== #t y))
	      (else U))
	     (== (list x y) r)))
; > ((tea #t) (cup #t) (#f #t))

; 
(run* (r)
      (fresh (x y)
	     (== x y)
	     (== (list x y) r)))
; > (_.0 _.0)

; Solutions are pairs of y and z.
; If the first answer succeeds, x and y co-refer,
; and z co-refers with a new fresh variable. Therefore
; y and z are different variables and dont co-refer.
; The second answer succeeds, when a fresh variable
; with y co-refers, and z and y co-refer. Therefore
; y and z are different variables and dont co-refer.
(run* (r)
      (fresh (x y z)
	     (conde
	      ((== y x) (fresh (x) (== z x)))
	      ((fresh (x) 
		      (== y x)) (== z x))
	      (else U))
	     (== (list y z) r)))
; > ((_.0 _.1) (_.0 _.1))

; But both occurences of _.0 represent different variables.
(run* (r)
      (fresh (x y z)
	     (conde
	      ((== y x) (fresh (x) (== z x)))
	      ((fresh (x) 
		      (== y x)) (== z x))
	      (else U))
	     (== #f x)
	     (== (list y z) r)))
; > ((#f _.0) (_.0 #f))

; Only the goal b is of interest.
(run* (q)
      (let ((a (== q #t))
	    (b (== q #f)))
	b))
; > (#f)

; Here is also only the goal b of interest.
; Whooo, very interesting.
(run* (q)
      (let ((a (== #t q))
	    (b (fresh (x)
		      (== x q)
		      (== #f x)))
	    (c (conde
		((== #t q) S)
		(else (== #f q)))))
	b))

;;;
'()
