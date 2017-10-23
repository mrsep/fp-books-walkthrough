#lang br/quicklang
(require "struct.rkt" "run.rkt" "elements.rkt")
(provide (rename-out [basic-module-begin #%module-begin])
         (all-from-out "elements.rkt"))

; instead of parse-tree, directly unpack the b-program node into
; a series of LINE ... arguments. This safes to define a b-program macro
(define-macro (basic-module-begin (b-program LINE ...))
  (with-pattern
    ([((b-line NUM STATEMENT ...) ...) #'(LINE ...)] ; pair (NUM ...) with a list of line numbers
     [(LINE-FUNC ...) (prefix-id "line-" #'(NUM ...))])
    #'(#%module-begin
       LINE ...
       (define line-table
         (apply hasheqv (append (list NUM LINE-FUNC) ...)))
       (void (run line-table)))))

