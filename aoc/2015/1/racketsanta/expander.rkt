#lang br/quicklang

(define-macro (santa-module-begin PARSE-TREE)
  #'(#%module-begin PARSE-TREE))

(provide (rename-out [santa-module-begin #%module-begin]))

(define-macro (santa-program LEFT-OR-RIGHT ...)
  #'(displayln (+ LEFT-OR-RIGHT ...)))

(define-macro (left-paren STR)
  #'1)

(define-macro (right-paren STR)
  #'-1)

(define-macro (closed-paren STR)
  #'0)

(provide santa-program)
(provide left-paren)
(provide right-paren)
