#lang br
(require "struct.rkt")
(provide b-line)

(define-macro (b-line NUM STATEMENT ...)
  (with-pattern ([LINE-NUM (prefix-id "line-" #'NUM
                                      #:source #'NUM)])
    (syntax/loc caller-stx ; caller-stx is the original input to the macro
      (define (LINE-NUM) (void) STATEMENT ...))))

