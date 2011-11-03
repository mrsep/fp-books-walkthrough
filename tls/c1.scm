;; The Little Schemer
;; Chapter 1 - Toys

;; First, some Toys to play with: 
;; Atoms and Lists of Atoms, called a lat
;; Some primitive Functions: atom?, car, cdr, null?, cons, eq?

; What is an atom?
(define atom? 
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; The Laws of car and cdr
;; 
;; The primitives car and cdr are only defined for non-empty lists.
;; The cdr of any non-empty list is a list.

;; The Law of Null?
;;
;; The primitive is only defined for lists.

;; The Law of cons
;;
;; The primitive cons takes two arguments.
;; The second argument to cons must be a list.
;; The result is a list.

;; The Law of eq?
;;
;; The primitive takes two arguments
;; Each must be a non-numeric atom.


;; Chapter 2 - Do it, Do it Again, and Again, and Again ...

; "are all S-Expressions of the list x atoms?"
(define lat?
  (lambda (x)
    (cond 
     ((null? x) true)
     ((atom? (car x)) (lat? (cdr x)))
     (else false))))

; "is a a member of the list lat?"
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) false)
     (else (or (eq? a (car lat))
	       (member? a (cdr lat)))))))


;; Chapter 3 - Cons the Magnificent

; "remove the first occurence of a in the list lat"
; (rember a lat) = (filter #'(not (eq a %) lat))
(define rember
  (lambda (a lat)
    (cond 
     ((null? lat) '()) 
     ((eq? a (car lat)) (cdr lat))
     (else (cons (car lat) (rember a (cdr lat)))))))

;; (firsts l) = (mapcar car l)
(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l)) (firsts (cdr l)))))))

(define seconds
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (cdr l)) (seconds (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond 
     ((null? lat) '())
     ((eq? old (car lat)) (cons old
				(cons new (cdr lat))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new lat))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or
       (eq? o1 (car lat))
       (eq? o2 (car lat)))
      (cons new (cdr lat)))
     (else (cons (car lat) (subst2 new o1 o1 (cdr lat)))))))

;; "remove all occurences of a in the list lat"
;; (rember a lat) = (filter #'(not (eq a %) lat))
(define multirember
  (lambda (a lat)
    (cond 
     ((null? lat) '())
     ((eq? a (car lat)) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons old (cons new 
					  (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (crd lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new (cons old
					  (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))


(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new 
				(multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat)))))))
