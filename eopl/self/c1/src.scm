;;; Chapter 1 - Infuctive Sets of Data

;; 1.2 Deriving recursive Programs

; The Smaller-Subproblem Principle
; ================================
; If we can reduce a prog=blem to a smaller subproblem, we can
; call theprocedure that solved th problem to solve the subproblem.

; list-length
; list -> Int
(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        (1+ (list-length (cdr lst))))))
; Problem: no tail recursion!

; nth-element
; list -> Int -> list
(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (throw 'list-too-short 0)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (1- n))))))

; remove first occurence of a symbol in a list
; list -> symbol -> list
(define remove-first
  (lambda (lst symbol)
    (if (null? lst)
        lst
        (if (eqv? (car lst) symbol)
            (cdr lst)
            (cons (car lst)
                  (remove-first (cdr lst) symbol))))))
; Problem: no tail recursion!

; remove all occurences of a symbol in a list
; list -> symbol -> list
(define remove
  (lambda (lst symbol)
    (if (null? lst)
        lst
        (if (eqv? (car lst) symbol)
            (remove (cdr lst) symbol)
            (cons (car lst)
                  (remove (cdr lst) symbol))))))
; Problem: no tail recursion!

(define occurence?)

