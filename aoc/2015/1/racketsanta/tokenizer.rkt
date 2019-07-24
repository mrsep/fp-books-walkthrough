#lang br/quicklang
(require brag/support)

(define (make-tokenizer port)
  (port-count-lines! port)
  (define (next-token)
    (define santa-lexer
      (lexer
       [(eof) eof]
       [(:>= 1 "()") (next-token)]
       [(:>= 1 ")(") (next-token)]
       [(char-set "(")
        (token 'LEFT lexeme
               #:position (pos lexeme-start)
               #:line     (line lexeme-start)
               #:column   (col lexeme-start))]
       [(char-set ")")
        (token 'RIGHT lexeme
               #:position (pos lexeme-start)
               #:line     (line lexeme-start)
               #:column   (col lexeme-start))]
       [any-char (next-token)]))
      (santa-lexer port))
    next-token)

(provide make-tokenizer)
