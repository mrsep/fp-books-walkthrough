#lang br/quicklang
(require json)
; input: a parse tree
; '(jsonic-program
;  (jsonic-sexp " 42 ")
;  (jsonic-char "\n")
;  (jsonic-sexp " (* 6 7) "))

; output:
; - syntax objects with bindings and implementations of the nodes in the syntax tree
; - valid JSON

; entry point of the expander
(define-macro (jsonic-module-begin PARSE-TREE)
  #'(#%module-begin
     (define result-string PARSE-TREE) ; the parse tree will be converted by macros in a string
     (define validated-jsexpr (string->jsexpr result-string))
     ; no error happened; hence result-string is valid JSON
     (display result-string)))

(provide (rename-out [jsonic-module-begin #%module-begin]))

(define-macro (jsonic-program CHAR-OR-SEXP-STR ...)
  #'(string-trim (string-append CHAR-OR-SEXP-STR ...)))

(define-macro (jsonic-char CHAR-STR)
  #'CHAR-STR)

;; syntax template:
; A syntax object that appears within a macro definition or
; with-pattern form. Unlike an ordinary syntax object, a
; syntax template can refer to any defined pattern variables.
; These references are automatically replaced with the
; underlying matched value.

(define-macro (jsonic-sexp SEXP-STR)
  ; bind the result of format->datum to a pattern-variable usable in syntax-templates
  (with-pattern ; syntax template   vvvvvvvvvv   since we are in a macro (only a pattern variable)
    ([SEXP-DATUM (format-datum '~a #'SEXP-STR)]) ; convert the string into a S-exp
    ; convert the evaluated S-exp into a json-string into a syntax template
    #'(jsexpr->string SEXP-DATUM)))

(provide jsonic-program)
(provide jsonic-char)
(provide jsonic-sexp)
