#lang brag
b-program    : [b-line] (/NEWLINE [b-line])*
b-line       : b-line-num [b-statement] (/":" b-statement)* [b-rem]
@b-line-num  : INTEGER
@b-statement : b-end | b-print | b-goto
             | b-let | b-input | b-if
b-rem        : REM
b-end        : /"end"
b-let        : [/"let"] b-id /"=" (b-expr | STRING)
b-input      : /"input" b-id
b-print      : /"print" [b-printable] (/";" [b-printable])*
@b-printable : STRING | b-expr
b-goto       : /"goto" b-expr
@b-id        : ID ; splice and sets their b-id syntax property
; sum        : var ["+" sum] ; right to left evaluation -> (sum a (sum b (sum c)))
; sum        : [sum "+"] var ; left to right evaluation -> (sum (sum (sum a) b) c)

; condition and boolean expressions
b-if         : /"if" b-expr /"then" (b-statement | b-expr)
[/"else" (b-statement | b-expr)]

; expressions are ordered from lower to higher precedence
; sets of operations of same precedence:
b-expr       : b-or-expr
b-or-expr    : [b-or-expr "or"] b-and-expr
b-and-expr   : [b-and-expr "and"] b-not-expr
b-not-expr   : ["not"] b-comp-expr
b-comp-expr  : [b-comp-expr ("="|"<"|">"|"<>")] b-sum

; arithemtic expressions with order and precedence
; b-sum      : b-number (/"+" b-number)*
b-sum        : [b-sum ("+"|"-")] b-product
b-product    : [b-product ("*"|"/"|"mod")] b-neg
b-neg        : ["-"] b-expt
b-expt       : [b-expt "^"] b-value
@b-value     : b-number | b-id | /"(" b-expr /")"
@b-number    : INTEGER | DECIMAL
