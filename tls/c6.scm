;; The Little Schemer
;; Chapter 6 - Shadows

(load "c5.scm")

;; is aexp an arithmetic expression?
;; assume, that aexp is an arithmetic expression
;; test only, that numbers are at the right places
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((and
       (numbered? (car aexp))
       (numbered? (car (cdr (cdr aexp)))))))))

;; representation for infix notation used in value
(define operator
  (lambda (ae)
    (car (cdr ae))))

(define 1st-sub-exp
  (lambda (ae)
    (car ae)))

(define 2nd-sub-exp
  (lambda (ae)
    (car (cdr (cdr ae)))))

;; evaluate an arithmetic expression
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? '+   (operator nexp)) (+ (value (1st-sub-exp nexp))
				    (value (2nd-sub-exp nexp))))
     ((eq? '*   (operator nexp)) (* (value (1st-sub-exp nexp)) 
				    (value (2nd-sub-exp nexp))))
     ((eq? 'pow (operator nexp)) (pow (value (1st-sub-exp nexp))
				      (value (2nd-sub-exp nexp)))))))

;; primitives for numbers:
;; number? zero? add1, sub1
;; New representation for numbers
;; 4 == (() () () ()) [or eg 4 == ((((()))))]

(define sero?
  (lambda (n)
    (null? n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define up+
  (lambda (a b)
    (cond
     ((sero? b) a)
     (else (up+ (edd1 a) (zub1 b))))))

(define numb-repr-converter
  (lambda (n)
    (cond 
     ((zero? n) '())
     (else (cons '() (numb-repr-converter (sub1 n)))))))

;; Beware of the shadows!
;; in lat? or example
    
