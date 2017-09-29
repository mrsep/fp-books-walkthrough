#lang br/quicklang
(require brag/support)

(define (make-tokenizer port)
  (define (next-token)
    (define jsonic-lexer
      (lexer
       [(eof) eof]
       [(from/to "//" "\n") (next-token)] ; comment
       [(from/to "@$" "$@")               ; jsonic addon
        ; creates a token-structure with name SEXP-TOK
        (token 'SEXP-TOK (trim-ends "@$" lexeme "$@"))]
       [any-char (token 'CHAR-TOK lexeme)]))
    (jsonic-lexer port))
  next-token)

(provide make-tokenizer)
