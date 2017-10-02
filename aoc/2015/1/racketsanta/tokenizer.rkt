#lang br/quicklang
(require brag/support)

(define (make-tokenizer port)
  (define (next-token)
    (define santa-lexer
      (lexer
       [(eof) eof]
       [(:>= 1 "()") (next-token)]
       [(:>= 1 ")(") (next-token)]
       [(char-set "(")
        (token 'LEFT lexeme)]
       [(char-set ")")
        (token 'RIGHT lexeme)]
       [any-char (next-token)]))
      (santa-lexer port))
    next-token)

(provide make-tokenizer)
