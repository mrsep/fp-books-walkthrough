;; The Little Schemer
;; Chapter 10 - What is the Value of All of This?

(load "c9.scm")

; a entry is a pair of lists, the first is a set, both lists have equal length
; the idea is: the first list are keys to the corresponding value of the second

(define new-entry build)

(define look-up-entry
  (lambda (name entry entry-f)
    (look-up-entry-help name
			(first entry)
			(second entry)
			entry-f)))

(define look-up-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? name (car names)) (car values))
     (else
      (look-up-entry-help name 
			  (cdr names) 
			  (cdr values) 
			  entry-f)))))

; a table (= environment) is  a list of entries

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else (look-up-entry name 
			  (car table) 
			  (lambda (name)
			    (look-up-table name 
					   (cdr table) 
					   table-f)))))))


; we have the types (*const, *quote, *identifier, *lambda, *cond, *application)
; now we represent types as functions and call them actions
; the function value has to find out the type of expression it was passed and then 
; use the associated action

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e 'cons) *const)
     ((eq? e 'car) *const)
     ((eq? e 'cdr) *const)
     ((eq? e 'null?) *const)
     ((eq? e 'eq?) *const)
     ((eq? e 'atom?) *const)
     ((eq? e 'zero?) *const)
     ((eq? e 'add1?) *const)
     ((eq? e 'sub1?) *const)
     ((eq? e 'number?) *const)
     (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
     (else *application))))

; now define the interpreter-function value, like the function eval in scheme

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))


;; Actions do speak louder than words.

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

; the table memoizes the values of identifiers

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

; what's about non-primitive functions
; (lambda list-of-formal-arguments expression)

(define *lambda
  (lambda (e table)
    (build 'non-primitive
	   (cons table (cdr e)))))

; eval:
; (meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9))))
; --> (non-primitive ((((y z) ((8) 9))) (x) (cons x y)))

; some help functions for this (a closure record, see below and above)
(define table-of first)

(define formals-of second)

(define body-of third)

; now define *cond

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define question-of first)

(define answer-of second)

(define else?
  (lambda (e)
    (and 
       (atom? (car e))
       (eq? (car e) 'else))))

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines))) 
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines) table))
      (meaning (answer-of (car lines) table)))
     (else (evcon (cdr lines) table)))))

; now the *application action
; remember an application must allways determines the meaning of all its arguments

(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else (cons 
	    (meaning (car args) table)
	    (evlis (cdr args) table))))))

(define function-of car)

(define arguments-of cdr)

(define *application
  (lambda (e table)
    (apply (meaning (function-of e) table)
	   (evlis (arguments-of e) table))))

; the two representations of functions are:
; (primitive primitive-name)
; (non-primitive (table formals body))
; (table formals body) is called a closure record
; remember the help-functions {table,formals,body}-of for a closure record

(define primitive?
  (lambda (l)
    (eq? 'primitive (first l))))

(define non-primitive?
  (lambda (l)
    (eq? 'primitive (first l))))

(define apply
  (lambda (fun vals)
    (cond
     ((primitive? fun) 
      (apply-primitive (second fun) vals))
     ((non-primitive? fun) 
      (apply-closure (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name 'cons)
      (cons (first vals) (second vals)))
     ((eq? name 'car)
      (car (first vals)))
     ((eq? name 'cdr)
      (cdr (first vals)))
     ((eq? name 'null?)
      (null? (first vals)))
     ((eq? name 'eq?)
      (eq? (first vals) (second vals)))
     ((eq? name 'atom?)
      (:atom? (first vals)))
     ((eq? name 'zero?)
      (zero? (first vals)))
     ((eq? name 'add1)
      (add1 (first vals)))
     ((eq? name 'sub1)
      (sub1 (first vals)))
     ((eq? name 'number?)
      (number? (first vals))))))

(define :atom?
  (lambda (x)
    (or
     (atom? x)
     (null? x)
     (eq? (car x) 'primitive)
     (eq? (car x) 'non-primitive))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure) 
	     (extend-table
	      (new-entry (formals-of closure) vals)
	      (table-of closure)))))


;; Summary of
;; Chapter 10 - What is the Value of All of This?
;; or: Scheme in Scheme


; What is an apply?

; What is an closure?

; What is a function, primitive, non-primitive?

; What does meaning?

; What does value?

; Try it out!
; Don't forget quote before Lists!

; (define e '(atom? (quote (1 2))))

; (value e)
