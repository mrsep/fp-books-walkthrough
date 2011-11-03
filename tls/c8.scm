;; The Little Schemer
;; Chapter 8 - Lambda the Ultimate

;;; 9-th Commandment
;;; Abstract common patterns with a new function.

(load "c7.scm")

(define rember-f
  (lambda (pred x l)
    (cond
     ((null? l) '())
     ((pred x (car l)) (cdr l))
     (else (cons (car l) (rember-f pred x (cdr l)))))))

;; Shapes of Currying
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define rember-f
  (lambda (pred?)
    (lambda (x l)    
      (cond
       ((null? l) '())
       ((pred? x (car l)) (cdr l))
       (else (cons (car l) ((rember-f pred?) x (cdr l))))))))

(define insertR-f
  (lambda (pred?)
    (lambda (new old lat)
      (cond 
       ((null? lat) '())
       ((pred? old (car lat)) (cons old
				    (cons new (cdr lat))))
       (else (cons (car lat) ((insertR pred?) new old (cdr lat))))))))

(define insertL-f
  (lambda (pred?)
    (lambda (new old lat)
      (cond
       ((null? lat) '())
       ((pred? old (car lat)) (cons new lat))
       (else (cons (car lat) ((insertL pred?) new old (cdr lat))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons new (cons old l))))

(define insert-g
  (lambda (seq)
    (lambda (new old lat)
      (cond 
       ((null? lat) '())
       ((equal? old (car lat)) (seq new old (cdr lat)))
       (else (cons (car lat) ((insert-g seq) new old (cdr lat))))))))

;; some alternative definitions for insertions
(define insertR
  (insert-g seqR))

(define insertL
  (insert-g seqL))

(define insertL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst
  (insert-g seqS))

(define seqrem
  (lambda (new old l)
    l))

(define rember
  (lambda (a l)
    ((insert-g seqrem) false a l)))

;; shorter value function
(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x '+) +)
     ((eq? x '*) *)
     ((eq? x 'pow) pow))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      ((atom-to-function (operator nexp)) 
       (value (1st-sub-exp nexp))
       (value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (pred?)
    (lambda (a lat)
      (cond 
       ((null? lat) '())
       ((pred? a (car lat)) ((multirember-f pred?) a (cdr lat)))
       (else (cons (car lat) ((multirember-f pred?) a (cdr lat))))))))

(define multiremberT
  (lambda (pred? lat)
    (cond 
     ((null? lat) '())
     ((pred? (car lat)) (multiremberT pred? (cdr lat)))
     (else (cons (car lat) (multiremberT pred? (cdr lat)))))))

(define multirember&co
  (lambda (a lat col)
    (cond 
     ((null? lat) (col '() '()))
     ((eq? (car lat) a)
      (multirember&co a
		      (cdr lat)
		      (lambda (newlat seen)
			(col newlat
			     (cons (car lat) seen)))))
     (else
      (multirember&co a
		      (cdr lat)
		      (lambda (newlat seen)
			      (col (cons (car lat) newlat)
				   seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

;; some more stuff




;; my own stuff

(load "core.scm")

; flat list recursion funcion factory
(define mkfun-flat-list-recursion
  (lambda (base rec-op)
    (lambda (list)
      (cond
       ((null? list) base)
       (else (rec-op (car list) 
		     ((mkfun-flat-list-recursion base rec-op) (cdr list))))))))

; with some examples:

(example

  '(define length
     (mkfun-flat-list-recursion 0 
				(lambda (head result)
				  (add1 result))))
)
