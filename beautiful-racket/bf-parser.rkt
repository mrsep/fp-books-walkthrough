#lang brag

; grammar definition
bf-program : (bf-op | bf-loop)*
bf-op      : ">" | "<" | "+" | "-" | "." | ","
bf-loop    : "[" (bf-op | bf-loop)* "]"

; provides the two functions generating parse trees
; - parse: token-sequence -> syntax-object
; - parse-to-datum: token-sequence -> plain datum