#lang br/quicklang
(require "tokenizer.rkt" brag/support)

(define (read-syntax path port)
  (define tokens (apply-tokenizer make-tokenizer port))
  (strip-bindings
   #`(module basic-tokens-mod basic/tokenize-only
       #,@tokens)))

(module+ reader (provide read-syntax))

(define-macro (tokenize-only-mb TOKEN ...)
  #'(#%module-begin
     (list TOKEN ...)))

(provide (rename-out [tokenize-only-mb #%module-begin]))
