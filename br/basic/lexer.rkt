#lang br
(require brag/support)

(define-lex-abbrev reserved-terms (:or "print" "goto" "end"
                                       "input" "let"
                                       "=" "+" ":" ";"
                                       "-" "*" "/" "^" "mod" "(" ")"
                                       "if" "then" "else"
                                       "<" ">" "<>" "and" "or" "not"))
(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define basic-lexer
  (lexer-srcloc ; variant of lexer which automatically generates source locations
   [(eof) (return-without-srcloc eof)]
   ["\n" (token 'NEWLINE lexeme)]
   [whitespace (token lexeme #:skip? #t)] ; ignore this token in the parser
   [(from/stop-before "rem" "\n") (token 'REM lexeme)]
   [reserved-terms (token lexeme lexeme)] ; strings become tokens
   [(:seq alphabetic (:* (:or alphabetic numeric "$")))
    (token 'ID (string->symbol lexeme))]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:or (:seq (:? digits) "." digits)
         (:seq digits "."))
    (token 'DECIMAL (string->number lexeme))]
   [(:or (from/to "\"" "\"") (from/to "'" "'"))
    (token 'STRING
           (substring lexeme
                      1 (sub1 (string-length lexeme))))]))

(provide basic-lexer)