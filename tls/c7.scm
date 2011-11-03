;; The Little Schemer
;; Chapter 7 - Friends and Relations

(load "c6.scm")

(define set?
  (lambda (s)
    (cond
     ((null? s) true)
     (else (and
	    (not (member? (car s) (cdr s)))
	    (set? (cdr s)))))))


(define makeset
  (lambda (ms)
    (cond
     ((null? ms) '())
     (else
      (cons (car ms) 
	    (makeset (multirember (car ms) (cdr ms))))))))

;; is s1 a subset of s2?
(define subset?
  (lambda (s1 s2)
    (cond
     ((null? s1) true)
     (else (and
	    (member? (car s1) s2)
	    (subset? (cdr s1) s2))))))

(define eqset?
  (lambda (s1 s2)
    (and
     (subset? s1 s2)
     (subset? s2 s1))))

(define intersect?
  (lambda (s1 s2)
    (cond
     ((null? s1) false)
     (else (or
	    (member? (car s1) s2)
	    (intersect? (cdr s1) s2))))))

(define intersect
  (lambda (s1 s2)
    (cond
     ((null? s1) '())
     ((member? (car s1) s2) (cons (car s1) (intersect (cdr s1) s2)))
     (else (intersect (cdr s1) s2)))))

(define union
  (lambda (s1 s2)
    (cond
     ((null? s1) s2)
     ((member? (car s1) s2) (union (cdr s1) s2))
     (else (cons (car s1) (union (cdr s1) s2))))))

;; s1\s2
(define difference
  (lambda (s1 s2)
    (cond
     ((null? s1) '())
     ((member? (car s1) s2) (difference (cdr s1) s2))
     (else (cons (car s1) 
		 (difference (cdr s1) s2))))))

(define intersectall
  (lambda (ls)
    (cond
     ((null? (cdr ls)) (car ls))
     (else (intersect (car ls) 
		      (intersectall (cdr ls)))))))

;; representation of pairs

(define a-pair?
  (lambda (p)
    (cond
     ((atom? p) false)
     ((null? p) false)
     (else (and
	    (not (null? (cdr p)))
	    (null? (cdr (cdr p))))))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (x1 x2)
    (cons x1 (cons x2 '()))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

;; definition of a relation as a set of pairs
(define rel?
  (lambda (l)
    (cond
     ((null? l) true)
     (else (and
	    (a-pair? (car l))
	    (rel-b? (cdr l))
	    (set? l))))))

(define rel-b?
  (lambda (l)
    (cond
     ((null? l) true)
     (else (and
	    (a-pair? (car l))
	    (rel-b? (cdr l)))))))
	    

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else
      (cons (revpair (car rel))
	    (revrel (cdr rel)))))))

;; injective function?
(define fullfun?
  (lambda (rel)
    (set? (seconds rel))))

(define one-to-one?
  (lambda (rel)
    (fun? (revrel rel))))
