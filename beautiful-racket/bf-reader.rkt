#lang br/quicklang

(require "bf-parser.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define module-datum `(module bf-mod "bf-expander-functional.rkt"
                          ,parse-tree))
  (datum->syntax #f module-datum))
(provide read-syntax)

; tokenizer
; here: filters out non-relevant symbols
(require brag/support)
(define (make-tokenizer port)
  (define (next-token)
    (define bf-lexer
      (lexer
       [(eof) eof]
       [(char-set "<>-,.+[]") lexeme]
       [any-char (next-token)]))
    (bf-lexer port))
  next-token)