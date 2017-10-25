#lang br/quicklang
(require "parser.rkt" "tokenizer.rkt")

(define (read-syntax path port)
  (define parse-tree (parse (make-tokenizer port path)))
  (strip-bindings
   #`(module basic-mod basic/expander
       #,parse-tree)))

(module+ reader
  (provide read-syntax))

(define (get-info port mod line col pos)
  (define (handle-query key default)
    (case key
      [(color-lexer)
       (dynamic-require 'basic/colorer 'basic-colorer)]
      [else default]))
  handle-query)
