#lang br/quicklang

(provide + *)

(define-macro (stackerizer-mb EXPR)
  #'(#%module-begin
     (for-each displayln (reverse (flatten EXPR)))))

(provide (rename-out [stackerizer-mb #%module-begin]))

(define-macro (define-op OP)
  #'(define-macro-cases OP
      [(OP FIRST) #'FIRST]
      [(OP FIRST NEXT (... ...)) #'(list 'OP FIRST (OP NEXT (... ...)))])) ; macro recursion

; call macro writing a macro
(define-op +)
(define-op *)