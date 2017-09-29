#lang brag

; grammar definition
jsonic-program : (jsonic-char | jsonic-sexp)*
jsonic-char    : CHAR-TOK
jsonic-sexp    : SEXP-TOK
; The parser matches the named tokens, pulls the matched string out
; of the token and puts it in the parse tree, without the token name.
; Done.
