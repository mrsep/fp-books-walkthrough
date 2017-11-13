#lang br/quicklang
(require "struct.rkt" "run.rkt" "elements.rkt")
(provide (rename-out [basic-module-begin #%module-begin])
         (all-from-out "elements.rkt"))

; instead of parse-tree, directly unpack the b-program node into
; a series of LINE ... arguments. This safes to define a b-program macro
(define-macro (basic-module-begin (b-program LINE ...))
  (with-pattern
    ([((b-line NUM STATEMENT ...) ...) #'(LINE ...)] ; pair (NUM ...) with a list of line numbers
     [(LINE-FUNC ...) (prefix-id "line-" #'(NUM ...))]
     [(VAR-ID ...) (find-unique-var-ids #'(LINE ...))])
    #'(#%module-begin
       (define VAR-ID 0) ...
       LINE ...
       (define line-table
         (apply hasheqv (append (list NUM LINE-FUNC) ...)))
       (void (run line-table)))))

(begin-for-syntax
  (require racket/list)
  (define (find-unique-var-ids line-stxs)
    (remove-duplicates
     (for/list ([stx (in-list (stx-flatten line-stxs))]
                #:when (syntax-property stx 'b-id))
       stx)
     #:key syntax->datum)))
