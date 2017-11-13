#lang brag
b-program : [b-line] (/NEWLINE [b-line])*
b-line : b-line-num [b-statement] (/":" b-statement)* [b-rem]
@b-line-num : INTEGER
@b-statement : b-end | b-print | b-goto | b-let | b-input
b-rem : REM
b-end : /"end"
b-let : [/"let"] b-id /"=" (b-expr | STRING)
b-input : /"input" b-id
b-print : /"print" [b-printable] (/";" [b-printable])*
@b-printable : STRING | b-expr
b-goto : /"goto" b-expr
@b-id : ID ; splice and sets their b-id syntax property
; sum : var ["+" sum] ; right to left evaluation -> (sum a (sum b (sum c)))
; sum : [sum "+"] var ; left to right evaluation -> (sum (sum (sum a) b) c)

b-expr : b-sum
; b-sum : b-number (/"+" b-number)*
; sets of operations of same precedence:
b-sum : [b-sum ("+"|"-")] b-product
b-product : [b-product ("*"|"/"|"mod")] b-neg
b-neg : ["-"] b-expt
b-expt : [b-expt "^"] b-value
@b-value : b-number | b-id | /"(" b-expr /")"
@b-number : INTEGER | DECIMAL
