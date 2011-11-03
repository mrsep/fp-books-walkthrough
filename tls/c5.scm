;; The Little Schemer
;; Chapter 5 - *Oh My Gawd*: It's Full of Stars

(load "c4.scm")

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
		       ((eq? a (car l)) (rember* a (cdr l)))
		       (else (cons (car l) (rember* a (cdr l))))))
     (else (cons 
	    (rember* a (car l)) 
	    (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond 
     ((null? l) '())
     ((atom? (car l)) (cond
		       ((eq? old (car l)) 
			(cons old
			      (cons new (insertR* new old (cdr l)))))
		       (else (cons (car l) (insertR* new old (cdr l))))))
     (else (cons 
	    (insertR* new old (car l)) 
	    (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l)) (cond
		       ((eq? a (car l)) (add1 (occur* a (cdr l))))
		       (else (occur* a (cdr l)))))
     (else (op+ 
	    (occur* a (car l))
	    (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
		       ((eq? old (car l)) (cons new 
						(subst* new old (cdr l))))
		       (else (cons (car l) 
				   (subst* new old (cdr l))))))
     (else (cons (subst* new old (car l))
		 (subst* new old (cdr l)))))))

(define insertL*					   
  (lambda (new old l)
    ((cond
      ((null? l) '())
      ((atom? (car l)) (cond
			((eq? old (car l)) 
			 (cons new (cons old 
					 (insertL* new old (cdr l)))))
			(else (cons (car l) 
				    (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) 
		  (insertL* new old (cdr l))))))))

(define member*
  (lambda (a l)
    (cond 
     ((null? l) false)
     ((atom? (car l)) (or (eq? a (car l))
			  (member* a (cdr l))))
     (else (or (member* a (car l))
	       (member* a (cdr l)))))))

;; assume l is non-empty lists and does not contains empty lists
(define leftmost
  (lambda (l)
    (cond 
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))


;; Equality of Lists
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) true)
     ((or (null? l1) (null? l2)) false)
     (else (and 
	    (equal? (car l1) (car l2))
	    (eqlist? (cdr l1) (cdr l2)))))))

;; Equality of S-Expressions
(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) false)
     (else (eqlist? s1 s2)))))

;;; almost all functions that use eq? or = can be
;;; redefined using equal


;; "is a a member of the list lat?"
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) false)
     (else (or (equal? a (car lat))
	       (member? a (cdr lat)))))))

;; redefine rember for S-Expressions
(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? s (car l)) (cdr l))
     (else (cons (car l) (rember s (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond 
     ((null? lat) '())
     ((equal? old (car lat)) (cons old
				   (cons new (cdr lat))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((equal? old (car lat)) (cons new lat))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((equal? old (car lat)) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or
       (equal? o1 (car lat))
       (equal? o2 (car lat)))
      (cons new (cdr lat)))
     (else (cons (car lat) (subst2 new o1 o1 (cdr lat)))))))

;; "remove all occurences of a in the list lat"
;; (rember a lat) = (filter #'(not (eq a %) lat))
(define multirember
  (lambda (a lat)
    (cond 
     ((null? lat) '())
     ((equal? a (car lat)) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((equal? old (car lat)) (cons old (cons new 
					     (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (crd lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((equal? old (car lat)) (cons new (cons old
					     (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))


(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((equal? old (car lat)) (cons new 
				   (multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat)))))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((equal? a (car lat)) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))
