#lang br/quicklang

(define-macro (bf-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))

(provide (rename-out [bf-module-begin #%module-begin]))

; function signature of a bf-func
(define (bf-func arr ptr)
  ;; manipulate the array and ptr
  ;; return a list with two elements
  (list arr ptr))

(define (fold-funcs apl bf-funcs)           ; input: array and pointer list, list of bf-functions
                                            ; output: last accumulator state, i.e. array and pointer list
  (for/fold ([current-apl apl])             ; accumulator: array and pointer list
            ([bf-func (in-list bf-funcs)])  ; iterate over the list of bf-funcs
    (apply bf-func current-apl)))           ; loop body: applying the current bf-func to the current state
                                            ; the result is a two-list updating the accumulator

(define-macro (bf-program OP-OR-LOOP-ARG ...)
  #'(begin
      (define first-apl (list (make-vector 30000 0) 0))
      (void (fold-funcs first-apl (list OP-OR-LOOP-ARG ...)))))

(define-macro (bf-loop "[" OP-OR-LOOP-ARG ... "]")
  #'(lambda (arr ptr)
      (for/fold ([current-apl (list arr ptr)])
                ([i (in-naturals)]
                 #:break (zero? (apply current-byte
                                       current-apl)))
        (fold-funcs current-apl
                    (list OP-OR-LOOP-ARG ...)))))

; only return the name of a function (as a syntax-object?) so that fold-funcs can call apply on it
(define-macro-cases bf-op
  [(bf-op ">") #'gt]
  [(bf-op "<") #'lt]
  [(bf-op "+") #'plus]
  [(bf-op "-") #'minus]
  [(bf-op ".") #'period]
  [(bf-op ",") #'comma])

(provide bf-program)
(provide bf-loop)
(provide bf-op)

;; internal helper for manipulating local state
(define (current-byte arr ptr) (vector-ref arr ptr))

; This is pure functional but a little slow
;(define (set-current-byte arr ptr val)
;  (define new-arr (vector-copy arr))
;  (vector-set! new-arr ptr val)
;  new-arr)

; a lot faster, but non-pure
; however, it works here, since we never use the old values again
(define (set-current-byte arr ptr val)
  (vector-set! arr ptr val)
  arr)

(define (gt arr ptr) (list arr (add1 ptr)))
(define (lt arr ptr) (list arr (sub1 ptr)))

(define (plus arr ptr)
  (list
   (set-current-byte arr ptr (add1 (current-byte arr ptr)))
   ptr))

(define (minus arr ptr)
  (list
   (set-current-byte arr ptr (sub1 (current-byte arr ptr)))
   ptr))

(define (period arr ptr)
  (write-byte (current-byte arr ptr))
  (list arr ptr))

(define (comma arr ptr)
  (list
   (set-current-byte arr ptr (read-byte))
   ptr))
